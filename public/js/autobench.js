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
    return AB.plot.versions[AB.plot.version_by_series[seriesIndex]][time];
  }
  
  function getStdError(seriesIndex, event, time) {
    var impl = AB.plot.version_by_series[seriesIndex];
    var bm = AB.plot.element_benchmark[event.currentTarget.id];
    var err = AB.plot.errors[impl][bm][time][1];
    return err;
  }
  
  function getImplName(seriesIndex){
    var implspec = AB.plot.version_by_series[seriesIndex].split(',');
    return implspec[0] + " " + implspec[1];
  }
  
  return {
    clickedItem: null,
    hoveredItem: null,
    
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
            'click for more detail');
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
        //showTooltip(item.pageX, item.pageY, 'clicktip', "wheeee");
      } else {
        AB.mouse.clickedItem = null;
        $('.tooltip').remove();
      }
    },
    
    initListener: function(elt) {
//      elt.bind('plotclick', AB.mouse.click);
      elt.bind('plothover', AB.mouse.hover);
    }
  };
})();

AB.plot = (function(){
  function toDictionary(array){
    var ret = {};
    $.each(array, function(i, elt){
      ret[elt[0]] = elt;
    });
    return ret;
  }
  
  return {
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
    
    fetch: function(impl, mode, machine) {
      AB.plot.todo++;
      $.getJSON('/bench/json/data',
                {implementation: impl,
                 mode: mode,
                 host: machine},
                function(retrieved_data, status) {
                  if (status == 'success') {
                    // massage the data we retrieved:
                    var times = retrieved_data[0], stderrs = retrieved_data[1], versions = retrieved_data[2];
                    
                    AB.plot.errors[[impl, mode, machine]] = {};
                    AB.plot.versions[[impl, mode, machine]] = {};
                    
                    $.each(times, function(benchmark, timings){
                      if (AB.plot.benchmarks[benchmark])
                        AB.plot.benchmarks[benchmark]++
                      else
                        AB.plot.benchmarks[benchmark] = 1;
                      AB.plot.errors[[impl, mode, machine]][benchmark] = toDictionary(stderrs[benchmark]);
                      AB.plot.versions[[impl, mode, machine]] = versions;
                      retrieved_data[1][benchmark] = $.map(stderrs[benchmark],
                                                           function(err, n) {
                                                             return [[err[0], timings[n][1]+err[1],timings[n][1]-err[1]]];
                                                           });
                    });
                                        
                    // now store it, and draw it if this was the last outstanding request:
                    AB.plot.dataset[[impl, mode, machine]] = retrieved_data;
                  }
                  AB.plot.todo--;
                  if (AB.plot.todo == 0){
                    AB.plot.draw();
                  }
                });
    },
    
    draw: function(){
      $.each(AB.plot.benchmarks, function(benchmark, impls) {
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
      });
    },
  };
})();