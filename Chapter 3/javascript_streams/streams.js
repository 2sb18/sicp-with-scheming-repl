/* exported stream_enumerate_interval */
//
// let's try to implement 3.5.1 streams are delayed lists in javascript
//

// can we use a function as a delay? ya, I think so, so instead of writing 
//
// (cons-stream meow woof) we'd write [meow, function() { return woof; }]
//
//
//
// the_empty_stream is a specific object
var the_empty_stream = {};

function stream_car(stream) {
  "use strict";
  return stream[0];
}

function stream_cdr(stream) {
  "use strict";
  return stream[1]();
}

function stream_ref(s, n) {
  "use strict";
  if (n === 0) {
    return stream_car(s);
  } else {
    return stream_ref(stream_cdr(s), n - 1);
  }
}

function stream_map(proc, s) {
  "use strict";
  if (s === the_empty_stream) {
    return the_empty_stream;
  } else {
    return [proc(stream_car(s)), stream_map(proc, stream_cdr(s))];
  }
}

function stream_for_each(proc, s) {
  "use strict";
  if (s !== the_empty_stream) {
    proc(stream_car(s));
    stream_for_each(stream_cdr(s));
  }
}

function display_stream(s) {
  "use strict";
  stream_for_each(function(thing) {
    console.log(thing);
  }, s);
}

function stream_enumerate_interval(low, high) {
  "use strict";
  if (low > high) {
    return the_empty_stream;
  } else {
    return [low, function() {
      return stream_enumerate_interval(low + 1, high);
    }];
  }
}

var stream_enumerator = stream_enumerate_interval(0, 10);
