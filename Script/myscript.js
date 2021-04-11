Shiny.addCustomMessageHandler ('changeTxtColor',function (m) {
                      var element = $('#'+m.id); // Find element to change color of
                      element.css({ 'color': m.color}); // Change color of element
              });