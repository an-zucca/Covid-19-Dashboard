Shiny.addCustomMessageHandler ('changeVariableColor',function (m) {
  element = document.getElementById(m.id);
  element.style.color = m.color;
  });

Shiny.addCustomMessageHandler ('changeFormTitle',function (m) {
  document.getElementById(m.id).innerHTML = m.value;
});

Shiny.addCustomMessageHandler ('disabledButton',function (m) {
  document.getElementById(m.id).disabled = m.value;
});

Shiny.addCustomMessageHandler ('changeBorderColor',function (m) {
  var element = document.getElementById(m.id).closest('.box');
  element.style.borderTopColor = m.color;
});

Shiny.addCustomMessageHandler ('changeBgColor',function (m) {
  var element = document.getElementById(m.id);
  element.style.backgroundColor = m.color;
});
       
Shiny.addCustomMessageHandler ('changeNavBarColor',function (m) {
  var element = $('#tabset li');
  element.css({'border-top-color' : 'transparent'});
  var element = $('#tabset li.active');
  element.css({'border-top-color' : '#FCFCFF'});
  var element = $('#tabset li.active:nth-child(3)');
  element.css({'border-top-color' : m.color})
});

const array1 = ['Form1','Form2','Form3','Form4','Form5','Form6','Form7'];

array1.forEach(addListeners);

function addListeners(item) {
  
  document.getElementById('rowVal' + item).addEventListener("mouseover", function(){
    var abbrValue = document.getElementById(item + 'AbbrValue');
    var rawValue = document.getElementById(item + 'RawValue');
    
    if (abbrValue != null && rawValue != null) {
      abbrValue.style.display = 'none';
      rawValue.style.display = 'block';
    } 
  });
    
  document.getElementById('rowVal' + item).addEventListener("mouseout", function(){
    var abbrValue = document.getElementById(item + 'AbbrValue');
    var rawValue = document.getElementById(item + 'RawValue');
    
    if (abbrValue != null && rawValue != null) {
      abbrValue.style.display = 'block';
      rawValue.style.display = 'none';
    }
  });
  
  document.getElementById('rowVar' + item).addEventListener("mouseover", function(){
    var IncValue = document.getElementById(item + 'Inc');
    var PercIncValue = document.getElementById(item + 'PercInc');
    
    if (IncValue != null && PercIncValue != null) {
      IncValue.style.display = 'none';
      PercIncValue.style.display = 'block';
    }
  });
    
  document.getElementById('rowVar' + item).addEventListener("mouseout", function(){
    var IncValue = document.getElementById(item + 'Inc');
    var PercIncValue = document.getElementById(item + 'PercInc');
    
    if (IncValue != null && PercIncValue != null) {
      IncValue.style.display = 'block';
      PercIncValue.style.display = 'none';
    }
  });
                 
}