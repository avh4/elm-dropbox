Elm.Native.Dropbox = {};
Elm.Native.Dropbox.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Dropbox = elm.Native.Dropbox || {};
  if (elm.Native.Dropbox.values) return elm.Native.Dropbox.values;

  // Imports
  var Signal = Elm.Native.Signal.make(elm);

  function client(apiKey) {
    var client = new Dropbox.Client({ key: apiKey });

    client.authenticate(function(error, client) {
      if (error) {
        return alert(error);
      }
      console.log("elm-dropbox: Dropbox is authorized");
    });

    function read(filename) {
      var output = Signal.constant("");

      client.readFile(filename, function(error, data) {
        if (error) {
          return console.log("elm-dropbox: " + filename + ": " + error);
        }
        console.log("elm-dropbox: " + filename + ": Read from Dropbox");
        elm.notify(output.id, data);
      });

      return output;
    }

    function write(filename, dataSignal) {
      var isFirst = true;
      var writeToken = null;
      var handler = function(data) {
        if (isFirst) return isFirst = false;
        if (writeToken) clearTimeout(writeToken);
        writeToken = setTimeout(function() {
          client.writeFile(filename, data, function(error, stat) {
            if (error) {
              return alert(error);
            }
            console.log("elm-dropbox: " + filename + ": Wrote revision " + stat.versionTag);
          });
        }, 5000);
      }
      Signal.map(handler)(dataSignal);
    }

    return { read: read, write: F2(write) };
  }

  return elm.Native.Dropbox.values = {
    client: client
  };
};
