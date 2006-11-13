function selectedValues (elt) {
         var ret = new Array();
         for (var i = 0; i < elt.options.length; i = i + 1) {
             if (elt.options[i].selected)
                ret.push(elt.options[i].value);
         }
         return ret;
}

function containsElementWithValue(elt, selected) {
         for (var i = 0; i < elt.options.length; i = i + 1) {
            if (elt.options[i].value == selected)
               return i;
         }
         return false;
}

function implCallback (type, data, evt) {
         if (type == 'error')
            alert ('Error retrieving data from server: ' + data);
         else {
            var elt = dojo.byId('ONLY-RELEASE');
            var selected = elt.value;
            elt.innerHTML=data;
            if (false != containsElementWithValue(elt, selected))
               elt.value = selected;
         }
}


function hostCallback (type, data, evt) {
         if (type == 'error')
            alert ('Error retrieving data from server: ' + data);
         else {
            var elt = dojo.byId('IMPLEMENTATIONS');
            var selected = selectedValues(elt);
            elt.innerHTML=data;
            for (var i = 0; i < selected.length; i = i + 1) {
               var index;
               if (false != (index =
                         containsElementWithValue(elt, selected[i])))
                  elt.options[index].selected = true;
            }
         }
}