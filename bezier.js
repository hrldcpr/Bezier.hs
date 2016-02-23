(function(window, document, undefined) {

  function constant(x) {
    return function(t) {
      return x;
    }
  }

  function linear(f, g) {
    return function(t) {
      var p = f(t);
      var q = g(t);
      var l = [];
      for (i in p) l[i] = (1 - t)*p[i] + t*q[i];
      return l;
    };
  }

  var DT = 0.01;

  var canvas = document.getElementById('canvas');
  var ctx = canvas.getContext('2d');

  var points;
  var time;
  function draw(offset, length) {
    if (offset === undefined) offset = 0;
    if (length === undefined) length = points.length;

    var c;
    if (length === 1) c = constant(points[offset]);
    else {
      var a = draw(offset, length - 1);
      var b = draw(offset + 1, length - 1);
      c = linear(a, b);

      if (length > 2) {  // don't draw line if bezier already is one
        var p = a(time);
        var q = b(time);
        ctx.beginPath();
        ctx.moveTo(p[0], p[1]);
        ctx.lineTo(q[0], q[1]);
        ctx.stroke();
      }

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

  points = [[200, 200], [100, 500], [500, 500], [500, 100]];
  window.requestAnimationFrame(animate);

})(this, this.document);
