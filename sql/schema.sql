--
-- PostgreSQL database dump
--

\connect - "sbcl-arch"

SET search_path = public, pg_catalog;

--
-- TOC entry 15 (OID 513163)
-- Name: from_universal_time (integer); Type: FUNCTION; Schema: public; Owner: sbcl-arch
--

CREATE FUNCTION from_universal_time (integer) RETURNS timestamp with time zone
    AS 'select timestamp with time zone ''1970-01-01 GMT'' + cast( ($1-2208988800)||'''' as interval);'
    LANGUAGE sql;


--
-- TOC entry 16 (OID 513164)
-- Name: from_universal_time (bigint); Type: FUNCTION; Schema: public; Owner: sbcl-arch
--

CREATE FUNCTION from_universal_time (bigint) RETURNS timestamp with time zone
    AS 'select timestamp with time zone ''1970-01-01 GMT'' 
  + cast( ($1-2208988800)||'''' as interval);'
    LANGUAGE sql;


--
-- TOC entry 17 (OID 513165)
-- Name: from_universal_time (numeric); Type: FUNCTION; Schema: public; Owner: sbcl-arch
--

CREATE FUNCTION from_universal_time (numeric) RETURNS timestamp with time zone
    AS 'select timestamp with time zone ''1970-01-01 GMT'' + cast( ($1-2208988800)||'''' as interval);'
    LANGUAGE sql;


--
-- TOC entry 18 (OID 513166)
-- Name: to_universal_time (timestamp with time zone); Type: FUNCTION; Schema: public; Owner: sbcl-arch
--

CREATE FUNCTION to_universal_time (timestamp with time zone) RETURNS bigint
    AS 'select cast(extract(epoch from $1) as bigint)+2208988800 ;'
    LANGUAGE sql;


--
-- TOC entry 2 (OID 513207)
-- Name: machine; Type: TABLE; Schema: public; Owner: sbcl-arch
--

CREATE TABLE machine (
    name character varying(90) NOT NULL,
    "type" character varying(10)
);


--
-- TOC entry 3 (OID 513209)
-- Name: impl; Type: TABLE; Schema: public; Owner: sbcl-arch
--

CREATE TABLE impl (
    name character varying(90) NOT NULL,
    field_offset integer
);


--
-- TOC entry 4 (OID 513211)
-- Name: version; Type: TABLE; Schema: public; Owner: sbcl-arch
--

CREATE TABLE "version" (
    i_name character varying(90) NOT NULL,
    "version" character varying(90) NOT NULL,
    is_release boolean,
    release_date bigint
);


--
-- TOC entry 5 (OID 513213)
-- Name: result; Type: TABLE; Schema: public; Owner: sbcl-arch
--

CREATE TABLE result (
    v_name character varying(90) NOT NULL,
    v_version character varying(90) NOT NULL,
    b_name character varying(90) NOT NULL,
    m_name character varying(90) NOT NULL,
    seconds double precision,
    date bigint
);


--
-- TOC entry 6 (OID 1689983)
-- Name: benchmark; Type: TABLE; Schema: public; Owner: sbcl-arch
--

CREATE TABLE benchmark (
    name character varying(90) NOT NULL,
    unit character varying(90) DEFAULT 'seconds' NOT NULL
);


--
-- TOC entry 12 (OID 1691285)
-- Name: result_benchmark_idx; Type: INDEX; Schema: public; Owner: sbcl-arch
--

CREATE INDEX result_benchmark_idx ON result USING btree (b_name);


--
-- TOC entry 11 (OID 1691286)
-- Name: version_release_date_idx; Type: INDEX; Schema: public; Owner: sbcl-arch
--

CREATE INDEX version_release_date_idx ON "version" USING btree (release_date);


--
-- TOC entry 9 (OID 1691287)
-- Name: version_is_release_idx; Type: INDEX; Schema: public; Owner: sbcl-arch
--

CREATE INDEX version_is_release_idx ON "version" USING btree (is_release);


--
-- TOC entry 13 (OID 1697883)
-- Name: result_date_idx; Type: INDEX; Schema: public; Owner: sbcl-arch
--

CREATE INDEX result_date_idx ON result USING btree (date);


--
-- TOC entry 7 (OID 513215)
-- Name: machine_pkey; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY machine
    ADD CONSTRAINT machine_pkey PRIMARY KEY (name);


--
-- TOC entry 8 (OID 513217)
-- Name: impl_pkey; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY impl
    ADD CONSTRAINT impl_pkey PRIMARY KEY (name);


--
-- TOC entry 10 (OID 513219)
-- Name: version_pkey; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY "version"
    ADD CONSTRAINT version_pkey PRIMARY KEY (i_name, "version");


--
-- TOC entry 19 (OID 513221)
-- Name: $1; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY "version"
    ADD CONSTRAINT "$1" FOREIGN KEY (i_name) REFERENCES impl(name) ON UPDATE NO ACTION ON DELETE NO ACTION;


--
-- TOC entry 20 (OID 513225)
-- Name: result_x_machine; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY result
    ADD CONSTRAINT result_x_machine FOREIGN KEY (m_name) REFERENCES machine(name) ON UPDATE NO ACTION ON DELETE NO ACTION;


--
-- TOC entry 21 (OID 513229)
-- Name: result_x_impl; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY result
    ADD CONSTRAINT result_x_impl FOREIGN KEY (v_name, v_version) REFERENCES "version"(i_name, "version") ON UPDATE NO ACTION ON DELETE NO ACTION;


--
-- TOC entry 14 (OID 1689986)
-- Name: benchmark_pkey; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY benchmark
    ADD CONSTRAINT benchmark_pkey PRIMARY KEY (name);


--
-- TOC entry 22 (OID 1690056)
-- Name: result_x_benchmark; Type: CONSTRAINT; Schema: public; Owner: sbcl-arch
--

ALTER TABLE ONLY result
    ADD CONSTRAINT result_x_benchmark FOREIGN KEY (b_name) REFERENCES benchmark(name) ON UPDATE NO ACTION ON DELETE NO ACTION;



-- arch-tag: "325cce44-ff31-11d8-8b1b-000c76244c24"
