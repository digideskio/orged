var Orged = {};

Orged.messages = m.prop([]);

Orged.socket = new WebSocket("ws://" + window.location.host);
Orged.socket.onmessage = function (event) {
  Orged.messages(Orged.messages().concat([event.data]));
  m.redraw();
};

var App = {
  controller: function () {
    this.summary = m.prop("");

    m.request({method: "GET",
               url: "/summary",
               deserialize: function(v) { return v; }}).
      then(this.summary, console.error);

  },

  view: function (ctrl) {
    function post() {
      document.getElementById("button").setAttribute("disabled", true);
      m.request({method: "POST",
                 url: "/",
                 deserialize: function(v) { return v; }}).
        then(function () {
          document.getElementById("button").removeAttribute("disabled", false);
        }, console.error);
    }

    if (Orged.messages().length !== 0) {
      var clear = m(".clear", { onclick: function () { Orged.messages([]); } }, "clear");
    }

    return m("div",
             [m("button#button", { onclick: post }, "SYNC"),
              m("pre", ctrl.summary()),
              m("pre", [
                "Messages:\n-----\n" + Orged.messages().join("\n"),
                m("br"),
                clear
              ])

             ]);
  }
};


m.mount(document.body, App);
