if (!AB) var AB = {};

AB.mouse = (function() {
  function showTooltip(x, y, id, contents) {
    $('<div id="'+id+'" class="tooltip">' + contents + '</div>').css( {
      top: y + 5,
      left: x + 5,
    }).appendTo("body").fadeIn(200);
  }
    
  function formatTimeString(avg, stderr) {
    var log10 = function(n) { return Math.log(n) / Math.log(10) };

    var beforeDot = Math.max(log10(avg), 1);
    if (stderr != 0) {
      var afterDot = Math.ceil(log10(avg / stderr))+1;
      return avg.toString().substring(0, beforeDot+1+afterDot) +
             '&#xb1;' + stderr.toString().substring(0, beforeDot+1+afterDot);
    } else {
      return avg.toString();
    }
  }
  
  function getVersion(seriesIndex, time) {
    return AB.plot.versions[AB.plot.version_by_series[seriesIndex]][time][0];
  }
  
  function getSha1(seriesIndex, time) {
    return AB.plot.versions[AB.plot.version_by_series[seriesIndex]][time][1];
  }
  
  function getStdError(seriesIndex, event, time) {
    var impl = AB.plot.version_by_series[seriesIndex];
    var bm = AB.plot.element_benchmark[event.currentTarget.id];
    var err = AB.plot.errors[impl][bm][time];
    return err;
  }
  
  function getImplName(seriesIndex){
    var implspec = AB.plot.version_by_series[seriesIndex];
    var prettyName = $("input[name="+implspec+"] ~ label").text();
    return prettyName;
  }
  
  return {
    clickedItem: null,
    hoveredItem: null,
    
    clear: function(){
      $(".tooltip").remove();
      AB.mouse.clickedItem = null;
      AB.mouse.hoveredItem = null;
    },
    
    hover: function (event, pos, item) {
      if(item && !AB.mouse.clickedItem) {
        if (item.datapoint != AB.mouse.hoveredItem) {
          $("#hovertip").remove();
          AB.mouse.hoveredItem = item.datapoint;
          var stdError = getStdError(item.seriesIndex, event, item.datapoint[0]);

          showTooltip(item.pageX, item.pageY, 'hovertip', 
            getImplName(item.seriesIndex) + " " +
            getVersion(item.seriesIndex, item.datapoint[0]) + ": " + 
            formatTimeString(item.datapoint[1], stdError)+ '<br/>'+
            '<em>(click for details)</em>');
        }
      } else {
        AB.mouse.hoveredItem = null;
        $("#hovertip").remove();
      }
    },
  
    click: function (event, pos, item) {
      if(item && item.datapoint != AB.mouse.clickedItem) {
        $(".tooltip").remove();
        AB.mouse.clickedItem = item.datapoint;
        
        var stdError = getStdError(item.seriesIndex, event, item.datapoint[0]);
        var gitlink = "", zoomlink = "";
        var sha1 = getSha1(item.seriesIndex, item.datapoint[0]);
        if (sha1) {
          gitlink = '<a onclick="window.open(this.href); return false;" href="http://git.boinkor.net/gitweb/sbcl.git?a=commit;h='+sha1+'">[git]</a> ';
        }
        if (!AB.plot.zoomed) {
          zoomlink = '<a title="zoom in on this revision" href="javascript:AB.plot.zoomIn(\''+item.seriesIndex+'\', \''+getVersion(item.seriesIndex, item.datapoint[0])+'\')">Zoom</a> ';
        }        
        showTooltip(item.pageX, item.pageY, 'clicktip', 
          "<h4>"+getImplName(item.seriesIndex) + ' ' + getVersion(item.seriesIndex, item.datapoint[0]) + "</h4>"+
          "<p>"+formatTimeString(item.datapoint[1], stdError)+"</p>" + 
          '<p class="links">' + zoomlink + gitlink +"</p>");
      } else {
        AB.mouse.clickedItem = null;
        $('.tooltip').remove();
      }
    },
    
    initListener: function(elt) {
      elt.bind('plotclick', AB.mouse.click);
      elt.bind('plothover', AB.mouse.hover);
    }
  };
})();

AB.plot = (function(){
  function destructureImplSpec(string){
    return string.split(',');
  };
  
  return {
    zoomed: false,
    
    // [benchmark] -> element containing the graph of that benchmark
    benchmark_element: {},
    
    // [element] -> benchmark
    element_benchmark: {},
    
    // series number -> [impl, mode, host]
    version_by_series: {},
    
    // [[impl, mode, host]][benchmark] -> [times, error bar extents] (data set, ready to draw)
    dataset: {},
    
    // [[impl, mode, host]][benchmark][time] -> error (for hover info)
    errors: {},
    
    // [[impl, mode, host]][time] -> version (for hover info)
    versions: {},
    
    // [benchmark] -> number of impls
    benchmarks: {},
    
    // number of data sets to fetch before we can draw:
    todo: 0,
    
    waitbox: function(status) {
      $.modaldialog.success(status, {title: "This is going to take a little while", showClose: false, timeout: -1});
    },
    
    init: function() {
      AB.plot.waitbox('Fetching data...');
      $("input.impl[checked]").each(function(i, elt){
        AB.plot.fetch(destructureImplSpec(elt.id));
      });
    },
    
    zoomIn: function(series, release) {
      if (AB.plot.zoomed)
        return false;
        
      AB.mouse.clear();
      var implspec = AB.plot.version_by_series[series];
      AB.plot.zoomed = {errors: AB.plot.errors,
                        versions: AB.plot.versions,
                        dataset: AB.plot.dataset};
      AB.plot.errors = {}; AB.plot.versions = {}; AB.plot.dataset = {};
      AB.plot.waitbox('Zooming in on release ' + release + '...');
      $('#impl-selector').hide();
      $('a#zoom-out').show();
      AB.plot.fetch(implspec, release);
    },
    
    zoomOut: function() {
      if (!AB.plot.zoomed)
        return false;
      
      AB.mouse.clear();
      $.each(AB.plot.zoomed, function(key, val){
        AB.plot[key] = AB.plot.zoomed[key];
      });
      $('#impl-selector').show();
      $('a#zoom-out').hide();
      AB.plot.zoomed = false;
      AB.plot.waitbox('Zooming out...');
      AB.plot.draw();
    },
    
    selectionChanged: function() {
      // this refers to the checkbox.
      var implspec = destructureImplSpec(this.id);
      AB.userPrefs.setImplementationActive(implspec, this.checked);
      if (this.checked) {
        AB.plot.waitbox('Fetching data...');
        // just got selected. Fetch the data and plot it
        AB.plot.fetch(implspec);
      } else {
        delete AB.plot.errors[this.id];
        delete AB.plot.versions[this.id];
        delete AB.plot.dataset[this.id];
        AB.plot.draw();
      }
    },
    
    fetch: function(implspec, release) {
      implspec = $.isArray(implspec) ? implspec : destructureImplSpec(implspec);
      var impl=implspec[0], mode=implspec[1], machine=implspec[2];
      AB.plot.todo++;
      var data = {implementation: impl,
                  mode: mode,
                  host: machine};
      if (release)
        data.release = release;
      $.getJSON('/bench/json/data', data,
                function(retrieved_data, status) {
                  var result_data = [{}, {}];
                  if (status == 'success') {
                    // massage the data we retrieved:
                    var times = retrieved_data.timings, versions = retrieved_data.versions;
                    
                    AB.plot.errors[[impl, mode, machine]] = {};
                    AB.plot.versions[[impl, mode, machine]] = {};
                    
                    $.each(times, function(benchmark, timings){                      
                      if (AB.plot.benchmarks[benchmark])
                        AB.plot.benchmarks[benchmark]++;
                      else
                        AB.plot.benchmarks[benchmark] = 1;

                      var resultErrors = [];
                      var resultTimes = [];    
                      AB.plot.errors[[impl, mode, machine]][benchmark] = {};                  
                      var i = 0;
                      $.each(timings, function(i, timeAndError){
                        resultTimes[i] = [timeAndError[0], timeAndError[1]];
                        resultErrors[i] = [timeAndError[0], 
                                           timeAndError[1]+timeAndError[2], 
                                           timeAndError[1]-timeAndError[2]];
                        AB.plot.errors[[impl, mode, machine]][benchmark][timeAndError[0]] = timeAndError[2];
                      });
                      AB.plot.versions[[impl, mode, machine]] = versions;
                      result_data[0][benchmark] = resultTimes;
                      result_data[1][benchmark] = resultErrors;
                    });
                                        
                    // now store it, and draw it if this was the last outstanding request:
                    AB.plot.dataset[[impl, mode, machine]] = result_data;
                  }
                  AB.plot.todo--;
                  if (AB.plot.todo == 0){
                    AB.plot.draw();
                  }
                });
    },
    
    drawOne: function(benchmark){
      var elt = AB.plot.benchmark_element[benchmark];
      var data = [];
      var series_num = 1;
      $.each(AB.plot.dataset, function(implspec, data_by_benchmark){
        $.merge(data, [{data: data_by_benchmark[1][benchmark],
                        bars: {show: true, barWidth: 1, shadowSize: 0},
                        color: '#000000'},
                       {data: data_by_benchmark[0][benchmark],
                        lines: {show: true, shadowSize: 0, lineWidth: 1}, 
                        points: {show: true, pointSize: 1, shadowSize: 0}}]);
        AB.plot.version_by_series[series_num] = implspec;
        series_num += 2;
      });
      $.plot(elt, data, 
             { xaxis: { mode: "time" },
                grid: { hoverable: true, clickable: true }})
      $('#dialog').remove();      
    },
    
    draw: function(){
      AB.plot.version_by_series = {};
      $.each(AB.plot.benchmarks, function(benchmark, impls) {
        if (!AB.userPrefs.benchmarkHidden($('div[id='+benchmark+']')))
          AB.plot.drawOne(benchmark);
      });
    },
  };
})();

AB.userPrefs = (function(){
  var initialized = false;
  
  return {
    hiddenBenchmarks: [],
    
    init: function() {
      if(initialized) return;
      $.jStore.ready(function(ev, engine){
        engine.ready(function(ev, engine){
          // restore implementation selection if we have any selected:
          if ($.store('activatedImplementations')) {
            $('input.impl').each(function(i, input){
              input.checked = AB.userPrefs.implementationActive(input.name);
            });
          } else {
            $('input.impl').each(function(i, input){
              AB.userPrefs.setImplementationActive(input.name, input.checked);
            });
          }
          
          // restore viewed benchmarks:
          $('div.graph').each(function(i, div) {
            if (AB.userPrefs.benchmarkHidden(div)) {
              AB.userPrefs.removeFromDisplay(div, true);
            }
          });
          AB.userPrefs.regenerateHiddenList();
        });
      });
      initialized = true;
    },
        
    removeFromDisplay: function(elt, delayRedisplay) {
      var name = $(elt).attr('id');
      $(elt).addClass('hidden');
      if (!delayRedisplay)
        AB.userPrefs.regenerateHiddenList();
    },
    
    regenerateHiddenList: function(){
      $('ul#hidden-benchmarks li').remove();
      $('div.graph').each(function(i, div){
        var name = div.id;
        $('<li><a class="'+name+'" href="javascript:AB.userPrefs.showBenchmark($(\'div[id='+name+']\')); return false">'+name+'</a></li>').appendTo('ul#hidden-benchmarks');
      });
    },
    
    showAgain: function(elt) {
      $(elt).removeClass('hidden');
      $('ul#hidden-benchmarks li a[class='+$(elt).attr('id')+']').parent().remove();
      AB.plot.drawOne($(elt).attr('id'));
    },
    
    hideBenchmark: function(elt) {
      var name = $(elt).attr('id');
      var benchmarksHidden = $.evalJSON($.store('hideBenchmarks'));
      if (!benchmarksHidden) {
        benchmarksHidden = [];
      }
      $.merge(benchmarksHidden, [name]);
      $.merge(AB.userPrefs.hiddenBenchmarks, [name]);
      
      $.store('hideBenchmarks', $.toJSON(benchmarksHidden));
      AB.userPrefs.removeFromDisplay(elt);
    },
    
    showBenchmark: function(elt) {
      var name = $(elt).attr('id');
      var benchmarksHidden = $.evalJSON($.store('hideBenchmarks'));
      if (!benchmarksHidden) {
        benchmarksHidden = [];
      }
      benchmarksHidden = $.map(benchmarksHidden, function(elt, i){
        if(elt == name)
          return [];
        else
          return [elt];
      });
      AB.userPrefs.hiddenBenchmarks = $.map(benchmarksHidden, function(elt, i){
        if(elt == name)
          return [];
        else
          return [elt];
      });
      $.store('hideBenchmarks', $.toJSON(benchmarksHidden));
      AB.userPrefs.showAgain(elt);
    },
    
    benchmarkHidden: function(elt) {
      var name = $(elt).attr('id');
      var benchmarksHidden = $.evalJSON($.store('hideBenchmarks'));
      if (!benchmarksHidden) {
        return $.inArray(name, AB.userPrefs.hiddenBenchmarks) >= 0;
      }
      return $.inArray(name, benchmarksHidden) >= 0;
    },
    
    implementationActive: function(spec){
      return $.evalJSON($.store('activatedImplementations'))[spec];
    },
    
    setImplementationActive: function(spec, value){
      var active = $.evalJSON($.store('activatedImplementations'));
      if (!active) active = {};
      if (value) {
        active[spec] = true;
      } else {
        delete active[spec];
      }
      $.store('activatedImplementations', $.toJSON(active));
    }
  };
})();