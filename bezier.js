(function(window, document, undefined) {

  function constant(x) {
    return function(t) {
      return x;
    }
  }

  function linear(f, g) {
    var cache = {};
    return function(t) {
      var k = t.toFixed(3);
      if (!(k in cache)) {
        var p = f(t);
        var q = g(t);
        var l = [];
        for (i in p) l[i] = (1 - t)*p[i] + t*q[i];
        cache[k] = l;
      }
      return cache[k];
    };
  }

  var DT = 0.01;

  var canvas = document.getElementById('canvas');
  var ctx = canvas.getContext('2d');

  var points;
  var time;
  var beziers = {};
  function draw(offset, length) {
    if (offset === undefined) offset = 0;
    if (length === undefined) length = points.length;

    if (length === 0) return;

    var a = draw(offset, length - 1);  // TODO if offset>0 then this part has already been drawn?
    var b = draw(offset + 1, length - 1);

    var k = offset + ',' + length;
    var c;
    if (k in beziers) c = beziers[k];
    else {
      if (length === 1) c = constant(points[offset]);
      else c = linear(a, b);
      beziers[k] = c;
    }

    if (length > 2) {  // don't draw line if bezier already is one
      var p = a(time);
      var q = b(time);
      ctx.beginPath();
      ctx.moveTo(p[0], p[1]);
      ctx.lineTo(q[0], q[1]);
      ctx.stroke();
    }

    if (length > 1) {
      ctx.beginPath();
      for (var t = 0; t < 1 + DT; t += DT) {
        var p = c(t);
        ctx.lineTo(p[0], p[1]);
      }
      ctx.stroke();
    }

    var l = c(time);
    ctx.beginPath();
    ctx.arc(l[0], l[1], 5, 0, 2 * Math.PI);
    ctx.fill();

    return c;
  }

  function animate(t) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    time = (1 + Math.cos(t / 1000)) / 2;
    draw();
    window.requestAnimationFrame(animate);
  }

  canvas.addEventListener('click', function(event) {
    beziers = {};  // clear all caches
    points.push([event.offsetX, event.offsetY]);
  });

  points = [[100, 500], [500, 100]];
  window.requestAnimationFrame(animate);

})(this, this.document);
