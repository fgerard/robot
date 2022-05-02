var SVGWrap = this.SVGWrap = function(b, c, d) {
    var a = b.createSVGPoint();
    a.x = c;
    a.y = d;
    b = a.matrixTransform(b.getScreenCTM().inverse());
    return {
      x: b.x,
      y: b.y
    }
  },
  SVGAnimate = this.SVGAnimate = function(id, x0, y0, x1, y1, delta) {
    var e=document.getElementById(id);
    var trans="translate("+(x1-x0)+"px,"+(y1-y0)+"px)";
    e.animate([{transform:'translate(0px,0px)'},{transform:trans}],{duration:delta,iterations:1,fill:"forwards"});
  },
  GOOGlogin = this.GOOGlogin = function(b, c) {
    if (window.gapi) {
      var d = window.gapi.auth2.getAuthInstance(),
        a = new gapi.auth2.SigninOptionsBuilder;
      a.setPrompt("select_account");
      d.signIn(a).then(function(a) {
        var c = a.getBasicProfile();
        a = a.getAuthResponse(!0);
        b(c.getEmail(), a.id_token)
      })
    } else c("Google login requires internet connection")
  },
  GOOGlogout = this.GOOGlogout = function() {
    window.gapi.auth2.getAuthInstance().signOut()
  },
  handler = this.handler = function(b) {
    b.preventDefault();
    console.log("pasando por aca...2");
    console.log(b);
    return b.returnValue = "Do you really want to quit? changes will not be stored";
  },
  registerListener = this.registerListener = function() {
    window.addEventListener("beforeunload", handler)
  },
  removeListener = this.removeListener = function() {
    window.removeEventListener("beforeunload", handler)
  };
