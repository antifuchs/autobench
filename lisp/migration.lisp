(in-package :autobench)

(defun schema-has-migration-p (name)
  (with-db-connection ()
    (handler-case (not (zerop (query (:select (:count 'name)
                                              :from 'schema_version
                                              :where (:= 'name name))
                                     :single)))
      (database-error ()
        (format *debug-io* "No table schema_version found, creating...")
        (with-transaction (setup-schema-version)
          (query (:create-table "schema_version" ((name :type string :primary-key))))
          nil)))))

(defparameter *migrations* nil)

(defun perform-missing-migrations ()
  (with-db-connection ()
    (dolist (migration (sort (remove-if #'schema-has-migration-p *migrations* :key #'string) #'string<))
      (format *debug-io* "Migrating ~A~%" migration)
      (with-transaction ()
        (funcall migration)
        (query (:insert-into 'schema-version :set 'name (string migration)))))))

(defmacro defmigration (name &body body)
  `(pushnew (defun ,name ()
              (macrolet
                  ((run-query (&body args)
                     `(progn
                        (format *debug-io* "Performing ~A..." (first ',args))
                        (force-output *debug-io*)
                        (let ((start (get-internal-real-time)))
                          (query ,@args)
                          (format *debug-io* "done(~fs)~%" (/ (- (get-internal-real-time) start)
                                                              internal-time-units-per-second))))))
                ,@body))
            *migrations*))

;;;; Migrations for boinkmarks:

(defmigration 0-use-surrogate-keys
  "Use surrogate keys for builds: should speed up accesses to results."
  (run-query "cluster version_release_date_idx on version")
  (run-query "alter table version add version_id serial")

  ;; recreate tables with good names:
  (run-query "create table benchmarks as select name as benchmark_name, 1 as benchmark_version, unit as unit from benchmark")
  (run-query "create table machines as select name as machine_name, type as machine_type from machine")
  (run-query "create table versions as 
               select version_id, i_name as implementation_name, version as version_number, is_release, 
                      release_date as release_date, belongs_to_release
                 from version")
  (run-query "create table builds as 
               select version_id, mode
                 from build join version on v_name=i_name and v_version=version")
  (run-query "create table results as 
               select version_id, mode, m_name as machine_name, b_name as benchmark_name, 1 as benchmark_version,
                      from_universal_time(date) as result_date, seconds
                 from result join version on v_name=i_name and v_version=version")
  (run-query "create table implementations as 
               select v_name as implementation_name, mode, m_name as machine_name, true as show, false as show_by_default
                 from result
                group by v_name, mode, m_name
                order by v_name, mode, m_name")
    
  ;; Create indexes/PK constraints now:  
  (run-query "alter table builds add primary key (version_id, mode)")
  (run-query "alter table versions add primary key (version_id)")
  (run-query "create index release_subversions_idx on versions(implementation_name, belongs_to_release)")
  (run-query "create unique index version_uniqueness_idx on versions(implementation_name, version_number)")
  (run-query "alter table results add primary key (version_id, mode, result_date, benchmark_name, benchmark_version)")
  (run-query "alter table implementations add primary key (implementation_name, mode, machine_name)")
  (run-query "alter table benchmarks add primary key (benchmark_name, benchmark_version)")
  (run-query "alter table machines add primary key (machine_name)")

  ;; FK constraints, and we're done:
  (run-query "alter table implementations add constraint implementations_machine_fk foreign key (machine_name)
                    references machines(machine_name) on delete cascade deferrable initially deferred")
  (run-query "alter table versions alter implementation_name set not null, alter version_number set not null")
  (run-query "alter table builds add constraint builds_versions_fk foreign key (version_id)
                    references versions(version_id) on delete cascade deferrable initially deferred")
  (run-query "alter table results add constraint results_builds_fk foreign key (version_id, mode)
                    references builds(version_id, mode) on delete cascade deferrable initially deferred")
  (run-query "alter table results add constraint results_benchmarks_fk foreign key (benchmark_name, benchmark_version)
                    references benchmarks(benchmark_name, benchmark_version) deferrable initially deferred")
  (run-query "alter table results add constraint results_machines_fk foreign key (machine_name)
                    references machines(machine_name) deferrable initially deferred")
  (run-query "alter table results alter seconds set not null, alter machine_name set not null")

  ;; version_id sequence
  (let* ((start (query (:select (:+ 1 (:max 'version_id)) :from 'versions)
                       :single!))
         (query (format nil
                        "create sequence versions_version_id_seq owned by versions.version_id no maxvalue start ~A" start)))
    (run-query (:raw query)))
  (run-query "alter table versions alter version_id set default nextval('versions_version_id_seq')")
  
  
  ;; Now drop the irrelevant tables 
  (run-query "drop table machine, impl, benchmark, build, result, version, impl_support, machine_support cascade")

  ;; Set default-on impls
  (run-query "update implementations set show_by_default=true 
               where machine_name='baker' 
                     and implementation_name <> 'CLISP'
                     and mode IN ('(:ARCH :EMULATED-X86 :FEATURES NIL)', '(:ARCH :X86_64 :FEATURES NIL)', 
                                  '(:ARCH :EMULATED-X86)')")
  (run-query "update implementations set show=false 
               where machine_name <> 'baker'")
  (run-query "update versions set is_release=false
               where implementation_name='CMUCL' and belongs_to_release='19c' and version_number<>'19c'"))

(defmigration 1-add-commit-hash
  "Add the commit SHA1 for versions that we know"
  (run-query "alter table versions add version_code varchar(90)")
  (with-current-directory (getf (rest (find 'sbcl *implementations-to-build*
                                            :key #'first))
                                :directory)
    (with-input-from-program (s-sha1 *git-binary* "log"
                                     "--reverse" "--pretty=format:%H"
                                     "version.lisp-expr")
      (loop for sha1 = (read-line s-sha1 nil nil)
            while sha1
            do (with-input-from-program (s-version (script "show-sbcl-version")
                                                   sha1)
                 (let ((version (eval (read s-version))))
                   (format t "Updating version ~A=>~A~%" version sha1)
                   (run-query
                    (:update 'versions
                             :set 'version-code sha1
                             :where (:and
                                     (:= 'version-number version)
                                     (:is-null 'version-code))))))))))