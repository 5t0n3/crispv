# crispv

> *It's like CSV, but crunchier!* (???)

A small web app/microservice™ for converting CSV data to HTML tables.

## Requesting & Receiving Data

All interaction with `crispv` is done via a single HTTP endpoint, namely `/render`.
CSV data should be send via POST request, header row and all, without URL-encoding.
Here is a sample request using cURL, together with the expected HTTP response:

```sh
$ curl https://$CRISPV_ENDPOINT/render -d a,b,c\n1,2,3
<table class="crispv-table"><tr><th>a</th><th>b</th><th>c</th></tr><tr><td>1</td><td>2</td><td>3</td></tr></table>
```

If invalid CSV data is sent, the response will instead be "bad request," e.g.

```sh
# row length mismatch
$ curl https://$CRISPV_ENDPOINT/render -d a,b,c\n1,2
bad request
```

A full rendering request/response interaction is also illustrated in the following UML sequence diagram:

![UML diagram illustrating HTTP interaction with crispv server](img/uml-sequence.png)
