if (!AB) var AB = {};

AB.mouse = (function() {
  function showTooltip(x, y, id, contents) {
    $('<div id="'+id+'" class="tooltip">' + contents + '</div>').css( {
      position: 'absolute',
      display: 'none',
      top: y + 5,
      left: x + 5,
      border: '1px solid #fdd',
      padding: '2px',
      'background-color': '#fee',
      opacity: 0.80
    }).appendTo("body").fadeIn(200);
  }
    
  return {
    clickedItem: null,
    hoveredItem: null,
  
    formatTimeString: function(avg, stderr) {
      var log10 = function(n) { return Math.log(n) / Math.log(10) };

      var beforeDot = Math.max(log10(avg), 1);
      if (stderr != 0) {
        var afterDot = Math.ceil(log10(avg / stderr))+1;
        return avg.toString().substring(0, beforeDot+1+afterDot) + '&#xb1;' + stderr.toString().substring(0, beforeDot+1+afterDot);
      } else {
        return avg.toString();
      }
    },
    
    hover: function(versions, stdErrors) {
      return function (event, pos, item) {
        if(item && !AB.mouse.clickedItem) {
          if (item.datapoint != AB.mouse.hoveredItem) {
            $("#hovertip").remove();
            AB.mouse.hoveredItem = item.datapoint;
            var stdError = stdErrors[item.datapoint[0]];
            if (stdError)
              stdError = stdError[1]-stdError[2];
            else
              stdError = 0;
            showTooltip(item.pageX, item.pageY, 'hovertip', 
              versions[item.datapoint[0]][1] + " " + versions[item.datapoint[0]][0] + ": " + 
              AB.mouse.formatTimeString(item.datapoint[1], stdError));
          }
        } else {
          AB.mouse.hoveredItem = null;
          $("#hovertip").remove();
        }
      }
    },
  
    click: function(versions, errors) {
      return function (event, pos, item) {
        if(item && item.datapoint != AB.mouse.clickedItem) {
          $(".tooltip").remove();
          AB.mouse.clickedItem = item.datapoint;
          //showTooltip(item.pageX, item.pageY, 'clicktip', "wheeee");
        } else {
          AB.mouse.clickedItem = null;
          $('.tooltip').remove();
        }
      }
    }
  };
})();

AB.toDictionary = function(array){
  var ret = {};
  $.each(array, function(i, elt){
    ret[elt[0]] = elt;
  });
  return ret;
};

AB.plot = function(elt, name){
  $.getJSON('/bench/json/data',
            {benchmark: name, 
             implementation: 'SBCL',
             mode: '(:ARCH :X86_64 :FEATURES NIL)',
             host: 'baker'},
            function(data) {
              $.plot(elt, 
                     [{data: data[1],
                       bars: {show: true, barWidth: 1, shadowSize: 0},
                       color: '#000000'},
                      {data: data[0], 
                       lines: {show: true, shadowSize: 0, lineWidth: 1}, 
                       points: {show: true, pointSize: 1, shadowSize: 0}}], 
                     { xaxis: { mode: "time" },
                       grid: { hoverable: true, clickable: true }});
              elt.bind('plotclick', AB.mouse.click(data[2], AB.toDictionary(data[1])));
              elt.bind('plothover', AB.mouse.hover(data[2], AB.toDictionary(data[1])));
            });
};