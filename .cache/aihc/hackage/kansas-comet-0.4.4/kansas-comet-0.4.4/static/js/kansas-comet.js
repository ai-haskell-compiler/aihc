// Kansas Comet jQuery plugin
(function($) {
   var the_prefix = "";
   var kansascomet_session;
   var kansascomet_server;
   var eventQueues = {};
   var eventCallbacks = {};
   var please_debug = false;

   var debug = function () { };

   var failure_callback = function(ig,ty,msg,re) {
		console.error("redraw failed (retrying) : " + ig + "," + ty + "," + msg);
	}

   $.kc = {
   // If we want to debug, then add a true
   connect: function(prefix) {
      the_prefix = prefix;
      // if there is a ?debug=0, then send debug messages to it
     if (window.location.search == '?debug=1')  {
	console.log("using logging to console for " + prefix);
         debug = function(arg) {
	    console.log(arg);
         };
      }

          $.ajax({ url: the_prefix,
                    type: "POST",
                    data: "",
                    dataType: "script"});
      debug('connect(' + prefix + ')');
   },

   session: function(server_id, session_id) {
      kansascomet_server = server_id;
      kansascomet_session = session_id;
      debug('session(' + session_id + ')');
      $.kc.register("session","abort",null);
      $.kc.redraw(0);
   },

   // Set failure behavior
   failure: function(f) {
	failure_callback = f;
   },

   redraw: function (count) {
      debug('redraw(' + count + ') url = ' + the_prefix + "/act/" + kansascomet_server + "/" + kansascomet_session + "/" + count);
      $.ajax({ url: the_prefix + "/act/" + kansascomet_server + "/" + kansascomet_session + "/" + count,
                  type: "GET",
                  dataType: "script",
                  success: function success() { $.kc.redraw(count + 1); },
		  error: function failure(ig,ty,msg) { 
			failure_callback(ig,ty,msg,function() { $.kc.redraw(count + 1); });
		  }
             });
               // TODO: Add failure; could happen
        },
   // TODO: move register, send & waitFor into different library
   // This says someone is listening on a specific event
   // The full event name is "scope/eventname", for example
   // "body/click"
   register: function (scope, eventname, fn) {
      debug('register(' + scope + ',' + eventname + ')');
      var fulleventname = scope + "/" + eventname;
           eventQueues[fulleventname] = [];
	   if (fn == null) {
	       // no special setup required, because no callback to call.
	   } else {
               $(scope).on(eventname, "." + eventname, function (event,aux) {
                  var e = fn(this,event,aux);
                  debug('{callback}on(' + eventname + ')');
                  e.eventname = eventname;
                  $.kc.send(fulleventname,e);
              });
	   }
   },

   send: function (fulleventname, event) {
      debug('send(' + fulleventname + ')');
      if (eventCallbacks[fulleventname] == undefined) {
      		if (eventQueues[fulleventname] != undefined) { 
                   eventQueues[fulleventname].push(event);
		} else {
      		     debug('send(' + fulleventname + ') not sent (no one listening)');
		}
           } else {
                   eventCallbacks[fulleventname](event);
           }
   },
   
   // This waits for (full) named event(s). The second argument is the continuation
   waitFor: function (scope, eventnames, fn) {
      debug('waitFor(' + scope + ',' + eventnames + ')');
      var prefixScope = function(o) { return scope + "/" + o; }
      for (eventname in eventnames) {
         var e = eventQueues[prefixScope(eventnames[eventname])].shift();
         if (e != undefined) {
            // call with event from queue
            fn(e);
            // and we are done
            return;   
         }
         if (eventCallbacks[prefixScope(eventnames[eventname])] != undefined) {
                 alert("ABORT: event queue callback failure for " + eventname);
         }
      }
      // All the callback better be undefined
      var f = function (e) {
                   // delete all the waiting callback(s)
            for (eventname in eventnames) {
                delete eventCallbacks[prefixScope(eventnames[eventname])];
            }
                   // and do the callback
                   fn(e);
      };
      for (eventname in eventnames) {
          eventCallbacks[prefixScope(eventnames[eventname])] = f;
      }
   },
   // There is a requirement that obj be an object or array.
   // See RFC 4627 for details.
   reply: function (uq,obj) {
      debug('reply(' + uq + ')');
           $.ajax({ url: the_prefix + "/reply/" + kansascomet_server + "/" + kansascomet_session + "/" + uq,
                    type: "POST",
                    // This wrapper is needed because the JSON parser
                    // used on the Haskell side only supports objects
                    // and arrays. But the returned data might be just
                    // a number or a boolean. So this wrapper keeps 
                    // the value safe to parse and has to be unwrapped 
                    // on server side. Formatting it as string is also
                    // important for some reason.
                    data: "{ \"data\": " + $.toJSON(obj) + " }",
                    contentType: "application/json; charset=utf-8",
                    dataType: "json"});
   },
   event: function (obj) {
      debug('event(' + $.toJSON(obj) + ')');
           $.ajax({ url: the_prefix + "/event/" + kansascomet_server + "/" + kansascomet_session,
                    type: "POST",
                    // This wrapper is needed because the JSON parser
                    // used on the Haskell side only supports objects
                    // and arrays. But the returned data might be just
                    // a number or a boolean. So this wrapper keeps 
                    // the value safe to parse and has to be unwrapped 
                    // on server side. Formatting it as string is also
                    // important for some reason.
                    data: "{ \"data\": " + $.toJSON(obj) + " }",
                    contentType: "application/json; charset=utf-8",
                    dataType: "json"});
   }    
     };
})(jQuery);





