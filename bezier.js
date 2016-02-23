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

  function bezier(ps, offset, length) {
    if (offset === undefined) offset = 0;
    if (length === undefined) length = ps.length;

    if (length === 1) return constant(ps[offset]);
    else return linear(bezier(ps, offset, length - 1),
                       bezier(ps, offset + 1, length - 1));
  }

  var points = [[200, 200], [100, 500], [500, 500], [500, 100]];


  var canvas = document.getElementById('canvas');
  var ctx = canvas.getContext('2d');

  function draw(time, offset, length) {
    if (offset === undefined) offset = 0;
    if (length === undefined) length = points.length;

    if (length === 1) return;

    var p = draw(time, offset, length - 1);
    var q = draw(time, offset + 1, length - 1);
    if (length > 2) {  // don't draw line if bezier already is one
      ctx.beginPath();
      ctx.moveTo(p[0], p[1]);
      ctx.lineTo(q[0], q[1]);
      ctx.stroke();
    }

    var curve = bezier(points, offset, length);
    ctx.beginPath();
    for (var t = 0; t < 1; t += 0.01) {
      var p = curve(t);
      ctx.lineTo(p[0], p[1]);
    }
    ctx.stroke();

    var l = curve(time);
    ctx.beginPath();
    ctx.arc(l[0], l[1], 5, 0, 2 * Math.PI);
    ctx.fill();
    return l;
  }

  function animate(time) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    draw((1 + Math.cos(time / 1000)) / 2);
    window.requestAnimationFrame(animate);
  }
  window.requestAnimationFrame(animate);

})(this, this.document);
