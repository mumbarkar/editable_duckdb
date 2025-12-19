HTMLWidgets.widget({

  name: 'hotwidget',

  type: 'output',

  factory: function(el, width, height) {

    // Handsontable instance for this element
    // Vendor libraries (DOMPurify, moment, Pikaday, numbro) are loaded via htmlwidgets
    // dependencies declared in hotwidget.yaml
    var hot = null;

    // ensure element has an id for Shiny inputs
    if (!el.id) el.id = 'hotwidget_' + Math.random().toString(36).substr(2, 9);

    function colsFromX(x) {
      var colnames = x.colnames || [];
      var colclasses = x.colclasses || [];
      var cols = [];
      var ncol = x.ncol || colnames.length || (x.data ? x.data.length : 0);
      for (var i = 0; i < ncol; i++) {
        var cls = (colclasses[i] || '').toLowerCase();
        var type = (cls.indexOf('numeric') !== -1 || cls.indexOf('integer') !== -1) ? 'numeric' : 'text';
        cols.push({
          data: i,
          type: type
        });
      }
      return cols;
    }

    function rowsFromX(x) {
      var cols = x.data || [];
      var ncol = x.ncol || cols.length;
      var nrow = x.nrow || (cols && cols[0] ? cols[0].length : 0);
      var rows = [];
      for (var r = 0; r < nrow; r++) {
        var row = new Array(ncol);
        for (var c = 0; c < ncol; c++) {
          var col = cols[c] || [];
          var val = (typeof col[r] === 'undefined') ? null : col[r];
          row[c] = val;
        }
        rows.push(row);
      }
      return rows;
    }

    function sendEditToShiny(change, x) {
      // change is [row, prop, oldValue, newValue]
      var row0 = change[0];
      var prop = change[1];
      var oldVal = change[2];
      var newVal = change[3];

      // prop is column index when data is array-of-arrays
      var colIndex = (typeof prop === 'number') ? prop : parseInt(prop, 10);
      if (isNaN(colIndex)) colIndex = 0;

      var payload = {
        row: row0 + 1,
        col: colIndex + 1,
        colname: (x.colnames && x.colnames[colIndex]) ? x.colnames[colIndex] : null,
        oldValue: oldVal,
        value: newVal
      };

      if (HTMLWidgets.shinyMode && typeof Shiny !== 'undefined') {
        try {
          Shiny.setInputValue(el.id + '_cell_edit', payload, {priority: 'event'});
        } catch (e) {
          console.error('hotwidget: failed to send edit to Shiny', e);
        }
      }
    }

    return {
      renderValue: function(x) {
        var settings = {
          data: rowsFromX(x),
          colHeaders: x.colnames || undefined,
          columns: colsFromX(x),
          licenseKey: 'non-commercial-and-evaluation',
          rowHeaders: true,
          stretchH: 'all',
          manualColumnResize: true,
          contextMenu: true,
          theme: 'ht-master'
        };

        // merge user-supplied options
        if (x.options && typeof x.options === 'object') {
          for (var k in x.options) {
            if (Object.prototype.hasOwnProperty.call(x.options, k)) {
              settings[k] = x.options[k];
            }
          }
        }

        // create or update instance
        if (!hot) {
          // ensure container is empty
          el.innerHTML = '';
          hot = new Handsontable(el, Object.assign({}, settings, {
            afterChange: function(changes /*, source */) {
              if (!changes) return;
              for (var i = 0; i < changes.length; i++) {
                try {
                  sendEditToShiny(changes[i], x);
                } catch (e) {
                  console.error('hotwidget afterChange handler error', e);
                }
              }
            }
          }));
        } else {
          // update data quickly
          if (typeof hot.loadData === 'function') {
            hot.loadData(settings.data);
            hot.updateSettings({
              columns: settings.columns,
              colHeaders: settings.colHeaders
            });
          } else {
            hot.updateSettings(settings);
          }
        }
      },

      resize: function(width, height) {
        if (hot && typeof hot.render === 'function') {
          hot.render();
        }
      }

    };
  }
});
