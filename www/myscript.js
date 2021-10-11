Shiny.addCustomMessageHandler ('changeVariableColor',function (m) {
                      var element = $('#'+m.id); // Find element to change color of
                      element.css({'color': m.color}); // Change color of element
              });

       
Shiny.addCustomMessageHandler ('changeVarValue',function (m) {
                      document.getElementById(m.id).innerHTML = m.value;
              });
              
const array1 = ['CumCases','CumHealed','Deaths','NewPos','CurrCases','CurrHomeIs','CurrHospSympt', 'CurrIntCare', 'CurrHosp', 'Swabs'];

array1.forEach(myFunction);

function myFunction(item, index) {
  
  document.getElementById('rowVar' + item).addEventListener("mouseover", function(){
    document.getElementById(item + 'Inc').style.display = "none";
    document.getElementById(item + 'PercInc').style.display = "block";
  });
    
  document.getElementById('rowVar' + item).addEventListener("mouseout", function(){
    document.getElementById(item + 'PercInc').style.display = "none";
    document.getElementById(item + 'Inc').style.display = "block";
  });
  
}