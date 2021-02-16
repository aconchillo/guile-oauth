
# guile-oauth

guile-oauth is an OAuth client module for Guile. It supports the following
features:

- [OAuth 1.0a](https://oauth.net/1/): HMAC-SHA1 and PLAINTEXT
  signatures.

- [OAuth 2.0](https://oauth.net/2/): Authorization Code and
  Client Credentials grant types.

It depends on the following Guile version and modules:

- [Guile](https://www.gnu.org/software/guile/) >= 2.2.0.
- [GnuTLS](https://www.gnutls.org/) Guile bindings (for HTTPS support).
- [guile-gcrypt](https://notabug.org/cwebber/guile-gcrypt/) >= 0.3.0.
- [guile-json](https://github.com/aconchillo/guile-json/) >= 4.5.0.


# Installation

Download the latest tarball and untar it:

- [guile-oauth-1.0.1.tar.gz](http://download.savannah.gnu.org/releases/guile-oauth/guile-oauth-1.0.1.tar.gz)

If you are cloning the repository make sure you run this first:

    $ autoreconf -vif

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where `<guile-prefix>` should preferably be the same as your system Guile
installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (oauth oauth2))
    scheme@(guile-user)>

It might be that you installed guile-oauth somewhere differently than
your system's Guile. If so, you need to indicate Guile where to find
guile-oauth, for example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile


# Usage

## OAuth 1.0a

- (**oauth1-client-request-token** url credentials) : Obtain a request token
  from the server *url* for the given client *credentials* (see
  *make-oauth1-credentials*).

  **Returns** : a service reponse that includes the request token.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server. It includes the response and body as arguments.

- (**oauth1-client-authorization-url** url [request-token] #:params) : Returns a
  complete authorization URL given the server *url* and the previously obtained
  *request-token* response.

  - *#:params* : a list of additional parameters.

  **Returns** : an authorization URL the client should connect to in order to
  grant permissions and obtain a verification code.

- (**oauth1-client-access-token** url credentials request-token verifier
  #:method #:extra-headers #:signature) : Obtain an access token from the server
  *url* for the given client *credentials*, *request-token* response and
  *verifier*.

  - *#:method* : the HTTP method to request the access token (defaults to
    *'POST*).

  - *#:extra-headers* : a list of additional HTTP headers.

  - *#:signature* : the signature algorithm to use (defaults to
    *oauth1-signature-hmac-sha1*).

  **Returns** : a service response that includes the access token.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server. It includes the response and body as arguments.

- (**oauth1-client-http-request** url credentials access-token) : Access a
  server's protected resource *url* with the given client *credentials* and the
  previously obtined *access-token* response.

  - *#:method* : the HTTP method to request the access token (defaults to
    *'GET*).

  - *#:params* : a list of additional parameters.

  - *#:extra-headers* : a list of additional HTTP headers.

  - *#:signature* : the signature algorithm to use (defaults to
    *oauth1-signature-hmac-sha1*).

  **Returns** : a couple of values, the response and the body (as a string).

## OAuth 2.0

- (**oauth2-client-authorization-url** url client-id #:redirect-uri #:scopes
  #:params) : Returns a complete authorization URL given the server *url* and
  the *client-id*.

  - *#:redirect-uri* : the URL the user should be redirected after the user
    authorizes the application.

  - *#:scopes* : a list of scopes (given as strings).

  - *#:params* : a list of additional parameters.

  **Returns** : Returns a couple of values: the complete authorization URL and
  the internally auto-generated state. The authorization URL is the URL the
  client should connect to in order to grant permissions and obtain an
  authorization code.

- (**oauth2-client-access-token-from-code** url code #:client-id #:redirect-uri
  #:method #:params #:auth #:extra-headers) : Obtain an access token from the
  server *url* for the given *code* using an Authorization Code grant.

  - *#:client-id* : the client ID.

  - *#:redirect-uri* : the URL the user was redirected after the user authorizes
    the application.

  - *#:method* : the HTTP method to request the access token (defaults to
    *'POST*).

  - *#:params* : a list of additional parameters.

  - *#:auth* : an authorization header (see *oauth-http-basic-auth*).

  - *#:extra-headers* : a list of additional HTTP headers.

  **Returns** : an access token.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server. It includes the response and body as arguments.

- (**oauth2-client-access-token-from-credentials** url client-id client-secret
  #:auth-type #:method #:params #:extra-headers) : Obtain an access token from
  the server *url* using a Client Credentials grant.

  - *client-id* : the client ID.

  - *client-secret* : the client secret.

  - *#:auth-type* : the authentication method to use (*'header* or *'params*,
    defaults to *'header*).

  - *#:method* : the HTTP method to request the access token (defaults to
    *'POST*).

  - *#:params* : a list of additional parameters.

  - *#:extra-headers* : a list of additional HTTP headers.

  **Returns** : an access token.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server. It includes the response and body as arguments.

- (**oauth2-client-refresh-token** url #:client-id #:client-secret #:auth-type
  #:method #:params #:extra-headers) : Obtain an access token from the server
  *url* using a Client Credentials grant.

  - *#:client-id* : the client ID.

  - *#:client-secret* : the client secret.

  - *#:auth-type* : the authentication method to use (*'header* or *'params*,
    defaults to *'header*).

  - *#:method* : the HTTP method to request the access token (defaults to
    *'POST*).

  - *#:params* : a list of additional parameters.

  - *#:extra-headers* : a list of additional HTTP headers.

  **Returns** : an access token.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server. It includes the response and body as arguments.

- (**oauth2-client-refresh-token** url #:client-id #:client-secret #:auth-type
  #:method #:params #:extra-headers) : Obtain an access token from the server
  *url* using a Client Credentials grant.

  - *#:client-id* : the client ID.

  - *#:client-secret* : the client secret.

  - *#:auth-type* : the authentication method to use (*'header* or *'params*,
    defaults to *'header*).

  - *#:method* : the HTTP method to request the access token (defaults to
    *'POST*).

  - *#:params* : a list of additional parameters.

  - *#:extra-headers* : a list of additional HTTP headers.

  **Returns** : an access token.

- (**oauth2-client-http-request** url access-token #:method #:params
  #:extra-headers) : Access a server's protected resource @var{url} with the
  @var{access-token} previously obtained.

  - *#:method* : the HTTP method to request the access token (defaults to
    *'GET*).

  - *#:params* : a list of additional parameters.

  - *#:extra-headers* : a list of additional HTTP headers.

  **Returns** : a couple of values, the response and the body (as a string).

## Helpers

- (**oauth-parse-www-form-urlencoded** str #:optional charset) : Parses the
  given *str* string of name/value pairs as defined by the content type
  application/x-www-form-urlencoded and returns and association list. The keys
  and values in the association list are strings.

- (**oauth-http-basic-auth** username password) : Create an HTTP basic
  authorization credentials header. If username or password are false returns
  nil.

- (**make-oauth1-credentials** key secret) : Creates new client credentials.

- (**oauth1-credentials-key** credentials) : Returns the client key.

- (**oauth1-credentials-secret** credentials) : Returns the client secret.

- (**make-oauth1-response** token token-secret params) : Creates a new service
  provider response for the given token, token secret and additional service
  parameters.

- (**oauth1-response-token** response) : Returns the token from a service
  provider response.

- (**oauth1-response-token-secret** response) : Returns the token secret from
  a service provider response.

- (**oauth1-response-params** response) : Returns additional parameters
  returns by the service provider.


# Examples

## OAuth 1.0a: Twitter client

The following example details how to obtain the tweets of your Twitter home
timeline. A complete example is available as a web application under the
*examples* directory.

- Load the OAuth 1.0a module:

```
> (use-modules (oauth oauth1))
```

- Define our Twitter URLs and application credentials:

```
> (define *request-url* "https://api.twitter.com/oauth/request_token")
> (define *auth-url* "https://api.twitter.com/oauth/authorize")
> (define *access-url* "https://api.twitter.com/oauth/access_token")
> (define *home-timeline* "https://api.twitter.com/1.1/statuses/home_timeline.json")
> (define *credentials* (make-oauth1-credentials "key" "secret"))
```

  The *key* and *secret* are provided by Twitter once you register a new
  application at https://developer.twitter.com.

- Obtain a request token:

```
> (define request-token (oauth1-client-request-token *request-url* *credentials*))
```

- Connect to the following returned URL for authorizing the request token:

```
> (oauth1-client-authorization-url *auth-url* request-token)
```

  Here you will need to login to Twitter or simply authorize your
  application if you are already logged in.

- Obtain the access token that will allow us to access protected resources:

```
> (define access-token
    (oauth1-client-access-token *access-url* *credentials* request-token "verifier"))
```

  The *verifier* is the string given by Twitter in the previous step.

- Get your tweets:

```
> (oauth1-client-http-request *home-timeline* *credentials* access-token)
```

## OAuth 2.0: Reddit client

The following example details how to obtain a Reddit feed. A complete example is
available as a web application under the *examples* directory.

- Load the OAuth 2.0 module:

```
> (use-modules (oauth oauth2))
```

- Define Reddit URLs:

```
> (define *user-agent* "guile:guile-oauth:1.0.0 (by /u/aconchillo)")
> (define *access-url* "https://www.reddit.com/api/v1/access_token")
> (define *reddit-feed-url* "https://oauth.reddit.com/r/scheme/new")
```

- Obtain the access token that will allow us to access protected resources using
  Client Credentials grant:

```
> (define access-token
    (oauth2-client-access-token-from-credentials *access-url* "client-id" "secret"))
```

  The *client-id* and *secret* are provided by Reddit once you register a new
  application at https://www.reddit.com/prefs/apps.

- Get your reddit feed:

```
> (oauth2-client-http-request *reddit-feed-url* access-token
                              #:extra-headers `((user-agent . ,*user-agent*)))
```

  The *user-agent* is a header required by Reddit.

# License

Copyright (C) 2013-2020 Aleix Conchillo Flaqué <aconchillo@gmail.com>

guile-oauth is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

guile-oauth is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with guile-oauth. If not, see https://www.gnu.org/licenses/.
