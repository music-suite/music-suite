


MIDI.loadPlugin({
    instruments: [ "acoustic_grand_piano" ], // or multiple instruments
    callback: function() {
        // console.log("Finished loading instrument.")
        
        // MIDI.Player.currentTime = 0; 
        // MIDI.Player.endTime     = 10; 
        // MIDI.Player.playing     = false;

        function loadRemote(path, callback) {
            var fetch = new XMLHttpRequest();
            fetch.open('GET', path);
            fetch.overrideMimeType("text/plain; charset=x-user-defined");
            fetch.onreadystatechange = function() {
                if(this.readyState == 4 && this.status == 200) {
                    /* munge response into a binary string */
                    var t = this.responseText || "" ;
                    var ff = [];
                    var mx = t.length;
                    var scc= String.fromCharCode;
                    for (var z = 0; z < mx; z++) {
                        ff[z] = scc(t.charCodeAt(z) & 255);
                    }
                    callback(ff.join(""));
                }
            }
            fetch.send();
        }
           
        // document.write("<div id='playback-title'></div>");
        // MIDI.Player.loadFile("6ac60822cc1e6066.mid", function() {
        //     console.log("Finished loading MIDI.");
        //     MIDI.Player.start();
        // }); 

        // loadRemote("4d4d02025ca6d59d.mid", function(data) {
            // MIDI.Player.loadFile("", function() {
                // console.log("Finished loading MIDI.");
                // MIDI.Player.start();
            // }); 
        // });
    }
});

var fixed = false;
function fixElem() {
    if (fixed) return;
    fixed = true;
    var el = document.createElement("div");
    el.innerHTML = "<div style='visibility:hidden' id='playback-title'></div>";
    document.body.appendChild(el);    
}

function stopPlaying() {
    fixElem();
    MIDI.Player.stop();
}
function playFile(file) {
    fixElem();
    stopPlaying();
    MIDI.Player.loadFile(file, function() {
        // console.log("Finished loading MIDI.");
        MIDI.Player.start();
    }); 
}



/*
    <script src="js/jasmid/stream.js"></script>
    <script src="js/jasmid/midifile.js"></script>
    <script src="js/jasmid/replayer.js"></script>
    <script src="js/midi.js"></script>
    <script src="js/Base64.js" type="text/javascript"></script>
    <script src="js/base64binary.js" type="text/javascript"></script>
    <script src="js/main.js" type="text/javascript"></script>
    


*/