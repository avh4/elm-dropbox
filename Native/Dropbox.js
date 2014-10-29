Elm.Native.Dropbox = {};
Elm.Native.Dropbox.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Dropbox = elm.Native.Dropbox || {};
  if (elm.Native.Dropbox.values) return elm.Native.Dropbox.values;

  // Imports
  var Signal = Elm.Native.Signal.make(elm);
  
  function read(apiKey, filename) {
    var output = Signal.constant("");

    var client = new Dropbox.Client({ key: apiKey });

    client.authenticate(function(error, client) {
      if (error) {
        return alert(error);
      }
      console.log("elm-dropbox: Dropbox is authorized");
      client.readFile(filename, function(error, data) {
        if (error) {
          return alert(error);
        }
        console.log("elm-dropbox: Read " + filename + " from Dropbox");
        elm.notify(output.id, data);
      });
    });

    return output;
  }

  return elm.Native.Dropbox.values = {
    read: F2(read)
  };
};
