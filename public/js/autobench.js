if (!AB) var AB = {};

AB.mouse = {
  hover: function (event, pos, item) {
    if(item){

    }
  },
  
  click: function (event, pos, item) {
    
  }
}

AB.plot = function(name){
  $.getJSON('/bench/json/data',
            {benchmark: name, 
             implementation: 'SBCL',
             mode: '(:ARCH :X86_64 :FEATURES NIL)',
             host: 'baker'},
            function(data) {
              $.plot($("#"+name), 
                         [{data: data[1],
                           bars: {show: true, barWidth: 1, shadowSize: 0},
                           color: '#000000'},
                          {data: data[0], 
                           lines: {show: true, shadowSize: 0, lineWidth: 1}, 
                           points: {show: true}}], 
                         { xaxis: { mode: "time" },
                           grid: { hoverable: true, clickable: true }});
              $('#chartdiv').bind('plotclick', AB.mouse.click);
              $('#chartdiv').bind('plothover', AB.mouse.hover);              
            });
}