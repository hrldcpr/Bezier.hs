
// # calls = 2 * curves + 1 = n * (n - 1) + 1 = 3, 7, 13, 21, 31, …
// # points = n + (n - 1) + … + 1 = n * (n + 1) / 2 = 3, 6, 10, 15, 21, …
// # curves = (n - 1) + (n - 2) + … + 1 = (n - 1) * n / 2 = 1, 3, 6, 10, 15, …
// # lines = (n - 2) + (n - 3) + … + 1 = (n - 2) * (n - 1) / 2 = 0, 1, 3, 6, 10, …

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

  var showAllPoints;
  var showPoints;
  var showAllLines;
  var showLines;
  var showAllCurves;
  var showCurve;
  var points;
  var time;
  var beziers = {};
  function draw(offset, length, dupe) {
    if (offset === undefined) offset = 0;
    if (length === undefined) length = points.length;

    var k = offset + ',' + length;
    var cached = k in beziers;
    var a, b, c;

    if (length > 1 && !(cached && dupe)) {
      a = draw(offset, length - 1, dupe || offset > 0);
      b = draw(offset + 1, length - 1, dupe);
    }

    if (k in beziers) c = beziers[k];
    else {
      if (length === 1) c = constant(points[offset]);
      else c = linear(a, b);
      beziers[k] = c;
    }

    if (!dupe) {
      // don't draw line if bezier already is one
      if ((showAllLines || (showLines && length === 2)) && length > 1 && !(showAllCurves && length === 2) && !(showCurve && points.length === 2)) {
        var p = a(time);
        var q = b(time);
        ctx.beginPath();
        ctx.moveTo(p[0], p[1]);
        ctx.lineTo(q[0], q[1]);
        ctx.stroke();
      }

      if (showAllCurves || (showCurve && length === points.length)) {
        ctx.beginPath();
        for (var t = 0; t < 1 + DT; t += DT) {
          var p = c(t);
          ctx.lineTo(p[0], p[1]);
        }
        ctx.stroke();
      }

      if (showAllPoints || (showPoints && length === 1)) {
        var l = c(time);
        ctx.beginPath();
        ctx.arc(l[0], l[1], 3, 0, 2 * Math.PI);
        ctx.fillStyle = 'cyan';
        ctx.fill();
      }
    }

    return c;
  }

  function animate(t) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    time = (1 + Math.cos(t / 1000)) / 2;
    draw();
    window.requestAnimationFrame(animate);
  }

  function setOptions() {
    showAllPoints = !location.hash.includes('!showAllPoints');
    showPoints = location.hash.includes('showPoints');
    showAllLines = !location.hash.includes('!showAllLines');
    showLines = location.hash.includes('showLines');
    showAllCurves = !location.hash.includes('!showAllCurves');
    showCurve = location.hash.includes('showCurve');
  }

  canvas.addEventListener('click', function(event) {
    beziers = {};  // clear all caches
    points.push([event.offsetX, event.offsetY]);
  });

  window.addEventListener('hashchange', setOptions);

  setOptions();
  points = [[300, 300]];
  window.requestAnimationFrame(animate);

})(this, this.document);
