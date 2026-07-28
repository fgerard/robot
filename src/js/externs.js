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
  GOOGdecodeJwt = function(idToken) {
    var b64 = idToken.split(".")[1].replace(/-/g, "+").replace(/_/g, "/");
    var binary = window.atob(b64);
    var bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return JSON.parse(new TextDecoder("utf-8").decode(bytes));
  },
  GOOGrenderButton = this.GOOGrenderButton = function(container, onSuccess, onError) {
    if (!(window.google && window.google.accounts && window.google.accounts.id)) {
      onError("Google login requires internet connection");
      return;
    }
    window.__googOnSuccess = onSuccess;
    window.__googOnError = onError;
    if (!window.__googInitialized) {
      google.accounts.id.initialize({
        client_id: document.querySelector('meta[name="google-signin-client_id"]').content,
        callback: function(response) {
          var email = GOOGdecodeJwt(response.credential).email;
          window.__googOnSuccess(email, response.credential);
        }
      });
      window.__googInitialized = true;
    }
    google.accounts.id.renderButton(container, {theme: "filled_black", size: "large", text: "signin_with", shape: "rectangular", width: 220});
  },
  GOOGlogout = this.GOOGlogout = function() {
    if (window.google && window.google.accounts && window.google.accounts.id) {
      google.accounts.id.disableAutoSelect();
    }
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
