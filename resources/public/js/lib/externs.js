var SVGWrap=this.SVGWrap=function(a,c,d){var b=a.createSVGPoint();b.x=c;b.y=d;a=b.matrixTransform(a.getScreenCTM().inverse());return{x:a.x,y:a.y}},SVGAnimate=this.SVGAnimate=function(a,c,d,b,e,f){document.getElementById(a).animate([{transform:"translate(0px,0px)"},{transform:"translate("+(b-c)+"px,"+(e-d)+"px)"}],{duration:f,iterations:1,fill:"forwards"})},GOOGlogin=this.GOOGlogin=function(a,c){if(window.gapi){var d=window.gapi.auth2.getAuthInstance(),b=new gapi.auth2.SigninOptionsBuilder;b.setPrompt("select_account");
d.signIn(b).then(function(b){var c=b.getBasicProfile();b=b.getAuthResponse(!0);a(c.getEmail(),b.id_token)})}else c("Google login requires internet connection")},GOOGlogout=this.GOOGlogout=function(){window.gapi.auth2.getAuthInstance().signOut()},handler=this.handler=function(a){a.preventDefault();console.log("pasando por aca...2");console.log(a);return a.returnValue="Do you really want to quit? changes will not be stored"},registerListener=this.registerListener=function(){window.addEventListener("beforeunload",
handler)},removeListener=this.removeListener=function(){window.removeEventListener("beforeunload",handler)};