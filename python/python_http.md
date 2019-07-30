# Basic Background Knowledge

## Overview

The Hypertext Transfer Protocol (HTTP) is an application protocol for distributed, collaborative, hypermedia information systems.

HTTP functions as a request–response protocol in the client–server computing model. The client submits an HTTP request message to the server. The server, which provides resources such as HTML files and other content, or performs other functions on behalf of the client, returns a response message to the client. The response contains completion status information about the request and may also contain requested content in its message body.

A web browser is an example of a _user agent_ (UA). Other types of user agent include the indexing software used by search providers (web crawlers), voice browsers, mobile apps, and other software that accesses, consumes, or displays web content.

```
            userinfo          host        port
        ________|_______ ______|_________ _|_
 http://john.doe:password@www.example.com:123/forum/questions/?tag=networking&order=newest#top
 -|--  ----------------|--------------------- ----------------|------------- -----------|------- 
 scheme         authority                      path                  query             fragment
```

HTTP/1.1 is a revision of the original HTTP (HTTP/1.0). In HTTP/1.0 a separate connection to the same server is made for every resource request. HTTP/1.1 can reuse a connection multiple times to download images, scripts, stylesheets, etc after the page has been delivered. HTTP/1.1 communications therefore experience less latency as the establishment of TCP connections presents considerable overhead.

An HTTP session is a sequence of network request-response transactions. An HTTP client initiates a request by establishing a Transmission Control Protocol (TCP) connection to a particular port on a server (typically port 80, occasionally port 8080). An HTTP server listening on that port waits for a client's request message. Upon receiving the request, the server sends back a status line, such as `HTTP/1.1 200 OK`, and a message of its own. The body of this message is typically the requested resource, although an error message or other information may also be returned. In HTTP/1.1 a keep-alive-mechanism was introduced, where a connection could be reused for more than one request. HTTP is a stateless protocol. A stateless protocol does not require the HTTP server to retain information or status about each user for the duration of multiple requests

## Message Format

The request message consists of 

- a request line

- request header fields

- an empty line

- an optional message body

with the request line and header fields ending with `<CR><LF>`.

HTTP defines methods to indicate the desired action to be performed on the identified resource.

The response message consists of 

- a status line which includes the status code and reason message

- response header fields

- an empty line

- an optional message body

The way the user agent handles the response depends primarily on the code, and secondarily on the other response header fields.  The first line of the HTTP response is called the status line and includes a numeric `status code` (such as "404") and a textual `reason phrase` (such as "Not Found"). HTTP status code is primarily divided into five groups for better explanation of request and responses between client and server as named.

- Informational `1xx`

- Successful `2xx`

- Redirection `3xx`

- Client Error `4xx`

- Server Error `5xx`

More practical information at https://code.tutsplus.com/tutorials/http-the-protocol-every-web-developer-must-know-part-1--net-31177




# `http.client` module

TODO

# `urllib` package

TODO

# `httplib2`

`httplib2` is a comprehensive HTTP client library.

`httplib2` supports caching, last-modified-checking (check the date to see if a cached resource should be resent), etag hashing (use hash value to determine if the file on the server has changed), compression (both deflate and gzip), redirects (automatically follows 3XX redirects on GETs).

`httplib2` doesn't automatically decode the content. It is up to the client to determine  which encoding the content is using, depending on the `Content-Type` header, the content itself and a fallback scheme.

You'll have to specify `no-cache` in `cache-control` field to bypass all the cache locally or on intermediate proxies.

`httplib2` automatically decompressed the body of the response and place it in the content.

## class `httplib2.Http`

The class that represents a client HTTP interface.

## class `httplib2.FileCache`

`FileCache` implements a Cache as a directory of files.

## class `httplib2.Response`

`Response` is a subclass of dict and instances of this class are returned from calls to `Http.request`.

# `requests`
