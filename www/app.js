// Browser side of the app. Years and dates come from window.appConfig
// (written by app.R): {min, max, calcStart}.

/* ---- charts: figureOutput() / renderFigure() ----
   The server sends {fig, dark, tall}: the light-theme plotly figure, the
   changes that turn it into the dark one (path + value pairs), and whether
   it needs the taller phone height. Light/dark is applied here, so the
   theme switch redraws at once without a trip to the server. */
(function(){
  function isDark(){ return document.documentElement.getAttribute('data-bs-theme') === 'dark'; }
  function draw(el){
    var x = el._figure;
    if(!x || !x.fig){ Plotly.purge(el); return; }
    var f = JSON.parse(JSON.stringify(x.fig));        // plotly keeps and changes what it is given
    if(isDark()) (x.dark || []).forEach(function(p){
      var o = f;
      for(var i = 0; i < p.p.length - 1; i++) o = o[p.p[i]];
      o[p.p[p.p.length - 1]] = p.v;
    });
    el.classList.toggle('tall', !!x.tall);
    Plotly.react(el, f.data, f.layout, f.config);
  }
  var binding = new Shiny.OutputBinding();
  $.extend(binding, {
    find: function(scope){ return $(scope).find('.figure-output'); },
    renderValue: function(el, x){ el._figure = x; draw(el); }
  });
  Shiny.outputBindings.register(binding, 'figure');
  new MutationObserver(function(){
    $('.figure-output').each(function(){ if(this._figure) draw(this); });
  }).observe(document.documentElement, {attributes: true, attributeFilter: ['data-bs-theme']});

  // a one-sentence summary of the chart for screen readers
  Shiny.addCustomMessageHandler('chart-label', function(m){
    var el = document.getElementById(m.id);
    if(el) el.setAttribute('aria-label', m.label);
  });
})();

/* ---- page state ---- */
$(document).on('shiny:connected', function(){
  // phone widths: fewer event notes on the growth chart
  Shiny.setInputValue('narrow', window.innerWidth < 640);
});

// the page title follows the market switch
function syncTitle(){
  var us = $('#country input:checked').val() === 'us';
  $('#app-title').text(us ? 'US Stock Market' : 'Australian Stock Market');
}
// the Valuation tab uses US data only: show it for the US, and leave it
// for Growth when switching to Australia
function syncValueTab(){
  var us = $('#country input:checked').val() === 'us';
  $('label[for=view_value]').toggle(us);
  if(!us && $('#view_value').is(':checked')) $('label[for=view_growth]').click();
}
// the Valuation tab has no years slider
function syncSlider(){ $('.years').toggle($('#view input:checked').val() !== 'value'); }
$(document).on('change', '#country input', function(){ syncTitle(); syncValueTab(); });
$(document).on('change', '#view input', function(){
  syncSlider();
  var l = document.querySelector('label[for=' + this.id + ']');
  if(l) l.scrollIntoView({block: 'nearest', inline: 'nearest', behavior: 'smooth'});
});
$(function(){ syncTitle(); syncValueTab(); syncSlider(); });

// never let the last visible series be switched off
$(document).on('click', '#show input', function(e){
  if(!this.checked && !$('#show input:checked').length) e.preventDefault();
});

/* ---- years: presets, the range text, the calculator's year selects and
   amount box all follow the slider (input$years) ---- */
(function(){
  var MIN = window.appConfig.min, MAX = window.appConfig.max, CALC_START = window.appConfig.calcStart;
  // the chart tabs and the calculator each keep their own years
  var ranges = {charts: [MIN, MAX], calc: [CALC_START, MAX]}, group = 'charts';
  function viewGroup(v){ return v === 'calc' ? 'calc' : 'charts'; }
  function slider(){ return $('#years').data('ionRangeSlider'); }
  function sync(){
    var s = slider(); if(!s) return;
    var a = s.result.from, b = s.result.to;
    ranges[group] = [a, b];
    $('#years-range').text(a === b ? a : a + '–' + b);
    $('.year-presets button').each(function(){
      var n = +this.dataset.span, on = n ? (b === MAX && a === MAX - n) : (a === MIN && b === MAX);
      this.classList.toggle('active', on); this.setAttribute('aria-pressed', on);
    });
    $('#calc_from').val(Math.min(a, MAX - 1)); $('#calc_to').val(Math.max(b, MIN + 1));
  }
  function setYears(a, b){
    var s = slider(); if(!s) return;
    s.update({from: a, to: b}); $('#years').trigger('change'); sync();
    // send now, not after the slider's usual delay, so a tab switch and its
    // years reach the server together (the slider's own later send is a repeat)
    if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('years', [a, b]);
  }
  $(document).on('change', '#years', sync);
  // switching between the charts and the calculator restores that side's
  // years (sent to the server together with the tab, so one redraw)
  $(document).on('change', '#view input', function(){
    var g = viewGroup(this.value);
    if(g === group) return;
    group = g; var r = ranges[g]; setYears(r[0], r[1]);
  });
  $(document).on('shiny:sessioninitialized', function(){
    // a bookmarked link can open on the calculator: its years are the slider's
    group = viewGroup($('#view input:checked').val());
    setTimeout(sync, 0);
  });
  $(document).on('click', '.year-presets button', function(){
    var n = +this.dataset.span; setYears(n ? MAX - n : MIN, MAX);
  });
  $(document).on('change', '#calc_from, #calc_to', function(){
    var a = +$('#calc_from').val(), b = +$('#calc_to').val();
    if(a >= b){ if(this.id === 'calc_from') b = Math.min(a + 1, MAX); else a = Math.max(b - 1, MIN); }
    setYears(a, b);
  });
  // amount: show thousands separators once typing stops
  $(document).on('blur', '#amount', function(){
    var v = parseFloat(this.value.replace(/[^0-9.]/g, ''));
    if(isFinite(v) && v > 0){ var c = v % 1 ? 2 : 0; this.value = v.toLocaleString('en-AU', {minimumFractionDigits: c, maximumFractionDigits: c}); }
  });
  $(document).on('keydown', '#amount', function(e){ if(e.key === 'Enter') this.blur(); });
})();
