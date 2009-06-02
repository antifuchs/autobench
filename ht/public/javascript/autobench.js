if (!AB) var AB = {};

AB.history = (function() {
  function nonElementPart(str) {
    return str.split(';')[1];
  }
  
  function elementPart(str) {
    var elt = str.split(';')[0];
    if (elt) return elt.substring(1);
  }
  
  function destructureImplReleaseSpec(spec) {
    var implSpecAndVersion = spec.split('/');
    if (implSpecAndVersion) {
      var implSpec = implSpecAndVersion[0].split(',');
      return [implSpecAndVersion[1], implSpec];
    }
  }
  
  return {
    changedTo: function(hash) {
      var eltPart = elementPart(hash);
      var elt;
      if (eltPart) elt = AB.plot.benchmark_element[elementPart(hash)];
      if (elt) {
        $.scrollTo(elt.parent().parent());
      }
      
      var zoomConfig = nonElementPart(hash);
      if (zoomConfig) {
        if (!AB.plot.initialized) AB.plot.skipInit = true;
        var releaseSpec = destructureImplReleaseSpec(zoomConfig);
        AB.plot.zoomIn(releaseSpec[1], releaseSpec[0]);
      } else if (AB.plot.zoomed) {
        AB.plot.zoomOut();
      }
      
      return false;
    },
    
    zoomInWithHistory: function(series, version) {
      var eltPart = elementPart(location.hash);
      if (!eltPart) eltPart = '';
      location.hash = eltPart + ';' + AB.plot.version_by_series[series] + '/' + version;
    },
    
    zoomOutWithHistory: function() {
      var eltPart = elementPart(location.hash);
      if (!eltPart) eltPart = '';
      location.hash = eltPart;
    },
    
    jumpToBenchmark: function(benchmarkName) {
      var versionSpec = nonElementPart(location.hash);
      location.hash = benchmarkName + ';' + versionSpec;
    },

    clearCurrentBenchmark: function() {
      var versionSpec = nonElementPart(location.hash);
      location.hash = ';' + versionSpec;
    }
  };
})();

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
  
  function canZoom(seriesIndex, time) {
    return AB.plot.versions[AB.plot.version_by_series[seriesIndex]][time][1];
  }
  
  function getSha1(seriesIndex, time) {
    return AB.plot.versions[AB.plot.version_by_series[seriesIndex]][time][2];
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
          gitlink = '<li><a onclick="window.open(this.href); return false;" href="http://git.boinkor.net/gitweb/sbcl.git?a=commit;h='+sha1+'">commit</a></li>';
        }
        if (!AB.plot.zoomed && canZoom(item.seriesIndex, item.datapoint[0])) {
          zoomlink = '<li><a title="show minor revisions after this release" href="javascript:AB.history.zoomInWithHistory(\''+item.seriesIndex+'\', \''+getVersion(item.seriesIndex, item.datapoint[0])+'\')">zoom</a></li>';
        }        
        showTooltip(item.pageX, item.pageY, 'clicktip', 
          '<h4><span class="impl">'+getImplName(item.seriesIndex) + '</span><span class="version">' + getVersion(item.seriesIndex, item.datapoint[0]) + "</span></h4>"+
          "<div><p>"+formatTimeString(item.datapoint[1], stdError)+"</p>" + 
          '<ul class="links">' + zoomlink + gitlink +"</ul></div>");
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
    skipInit: false,
    
    initialized: false,
    
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
      if (!AB.plot.skipInit) {
        AB.plot.waitbox('Fetching data...');
        $("input.impl").each(function(i, elt){
          if (elt.checked){
            AB.plot.fetch(destructureImplSpec(elt.id));
          }
        });
      }
      AB.plot.skipInit = false;
      AB.plot.initialized = true;
    },
    
    zoomIn: function(implspec, release) {
      if (AB.plot.zoomed)
        return false;
      
      AB.mouse.clear();
      AB.plot.zoomed = true;
      AB.plot.errors = {}; AB.plot.versions = {}; AB.plot.dataset = {};
      AB.plot.waitbox('Zooming in on release ' + release + '...');
      $('#zoom-release-spec').text(release);
      $('#impl-selector').hide();
      $('body').addClass('zoomed');
      AB.plot.fetch(implspec, release);
    },
    
    zoomOut: function() {
      if (!AB.plot.zoomed)
        return false;
      
      AB.mouse.clear();
      $('#impl-selector').show();
      $('body').removeClass('zoomed');
      AB.plot.init();
      AB.plot.zoomed = false;
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
      $.getJSON('/boinkmarks/json/data', data,
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
               yaxis: { min: 0},
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
    init: function() {
      if(initialized) return;
      
      // restore implementation selection if we have any selected:
      if ($.cookies.get('haveDefaults')) {
        $('input.impl').each(function(i, input){
          input.checked = AB.userPrefs.implementationActive(input.name);
        });
      } else {
        $.cookies.set('haveDefaults', 'yes');
        if (!$.cookies.test())
          alert("You have cookie paranoia settings that will negatively affect your user experience on this site.\nProceed with caution.");
        $('input.impl').each(function(i, input){
          AB.userPrefs.setImplementationActive(input.name, input.checked);
        });
      }
      
      // hide previously hidden benchmarks:
      var hidden = AB.userPrefs.hiddenBenchmarks();
      $('div.graph').each(function(i, div) {
        if (hidden[i] == '0') {
          AB.userPrefs.removeFromDisplay(div, true);
        }
      });
      AB.userPrefs.regenerateHiddenList();    
      initialized = true;
    },
        
    /// Benchmark visibility settings    
    
    removeFromDisplay: function(elt, delayRedisplay) {
      var name = $(elt).attr('id');
      $(elt).addClass('hidden');
      $(elt).removeClass('shown');
      if (!delayRedisplay)
        AB.userPrefs.regenerateHiddenList();
    },
    
    regenerateHiddenList: function(){
      $('ul#hidden-benchmarks li').remove();
      $('div.graph').each(function(i, div){
        var name = div.id;
        if (AB.userPrefs.benchmarkHidden(div))
          $('<li><a class="'+name+'" href="javascript:AB.userPrefs.showBenchmark($(\'div[id='+name+']\'))">'+name+'</a></li>').appendTo('ul#hidden-benchmarks');
      });
    },
    
    showAgain: function(elt) {
      $(elt).addClass('shown');
      $(elt).removeClass('hidden');
      $('ul#hidden-benchmarks li a[class='+$(elt).attr('id')+']').parent().remove();
      AB.plot.drawOne($(elt).attr('id'));
      AB.history.jumpToBenchmark($(elt).attr('id'));
    },
    
    updateHideCookieValue: function(){
      var cookieValue = '';
      $.each($('div.graph'), function(i, elt){
        cookieValue += ($(elt).hasClass('hidden') ? '0' : '1');
      });
      $.cookies.del('hide')
      $.cookies.set('hide', cookieValue);
    },
    
    hideBenchmark: function(elt) {
      AB.userPrefs.removeFromDisplay(elt);
      AB.userPrefs.updateHideCookieValue();
      AB.history.clearCurrentBenchmark();
    },
    
    showBenchmark: function(elt) {
      AB.userPrefs.showAgain(elt);
      AB.userPrefs.updateHideCookieValue();
    },
    
    benchmarkHidden: function(elt) {
      return $(elt).hasClass('hidden');
    },
    
    hiddenBenchmarks: function() {
      var val = $.cookies.get('hide');
      if (!val)
        val = '';
      return val;
    },
    
    /// Implementation checkbox settings
    
    implementationActive: function(spec){
      return $.cookies.get('impl_'+spec) == 'active';
    },
    
    setImplementationActive: function(spec, value){      
      if (value) {
        $.cookies.set('impl_'+spec, 'active');
      } else {
        $.cookies.del('impl_'+spec);
      }
    }
  };
})();