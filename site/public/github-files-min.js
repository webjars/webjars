(function(c){var d=[];
var a=[];
var b=function(f,i,g,h){if(f.data.content&&f.data.encoding==="base64"){var e=window.atob(f.data.content.replace(/\n/g,"")).split("\n");
g=g||e.length;
h(e.slice(i-1,g).join("\n"))
}};
c.getGithubFileByFilePath=function(e,g,f,j,i,h){if(d[f]){c.getGithubFile(e,g,d[f],j,i,h)
}else{c.ajax({type:"GET",url:"https://api.github.com/repos/"+e+"/"+g+"/contents/"+f,dataType:"jsonp",success:function(k){d[f]=k.data.sha;
c.getGithubFile(e,g,d[f],j,i,h)
}})
}};
c.getGithubFile=function(f,g,e,j,i,h){if(a[e]){b(a[e],+i||1,+h||0,j)
}else{c.ajax({type:"GET",url:"https://api.github.com/repos/"+f+"/"+g+"/git/blobs/"+e,dataType:"jsonp",success:function(k){a[e]=k;
b(a[e],+i||1,+h||0,j)
}})
}}
}(jQuery));