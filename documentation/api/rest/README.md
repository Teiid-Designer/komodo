# RAML Specification for vdb-builder
This details the Rest API available from the vdb-builder engine.

# Converting to HTML
Whilst the raml document is fairly easy-to-read, the html version provides a far better point-and-click environment.

To convert to html:

* Download and install *npm*.

* Install *raml2html* using the command (as root or Administrator):
```
npm install -g raml2html
```

* Execute raml2html on the raml file:
```
raml2html vdb-builder.raml > vdb-builder.html
```

