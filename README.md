
# guile-oauth

guile-oauth is a simple OAuth client module for Guile. It supports the
following features:

- [OAuth 1.0a](https://oauth.net/core/1.0a/) protocol support with HMAC-SHA1
  and PLAINTEXT signatures.

It depends on the following Guile version and modules:

- [Guile](https://www.gnu.org/software/guile/) >= 2.2.0.
- [GnuTLS](https://www.gnutls.org/) Guile bindings (for HTTPS support).
- [guile-gcrypt](https://notabug.org/cwebber/guile-gcrypt/) >= 0.3.0.
- [guile-json](https://github.com/aconchillo/guile-json/) >= 3.0.0 (only to run Twitter example).


# Installation

Download the latest tarball and untar it:

- [guile-oauth-0.5.0.tar.gz](http://download.savannah.gnu.org/releases/guile-oauth/guile-oauth-0.5.0.tar.gz)

If you are cloning the repository make sure you run this first:

    $ autoreconf -vif

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where <guile-prefix> should preferably be the same as your system Guile
installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (oauth oauth1))
    scheme@(guile-user)>

It might be that you installed guile-oauth somewhere differently than
your system's Guile. If so, you need to indicate Guile where to find
guile-oauth, for example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile


# Usage

## OAuth 1.0a

- (**oauth1-client-request-token** url credentials) : Obtain a request token
  from the server url for the given client credentials.

  **Returns** : a service response.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server.

- (**oauth1-client-authorization-url** url request-token) : Returns a complete
  authorization URL given the server url and a request token.

  **Returns** : an authorization URL the client should connect to in order to
  grant permissions and obtain a verification code.

- (**oauth1-client-access-token** url credentials request-token verifier) :
  Obtain an access token from the server url for the given client credentials,
  request token and verifier.

  **Returns** : a service response.

  **Throws**

  - *oauth-invalid-response* : if an unexpected response was returned from the
    server.

- (**oauth1-client-request** url credentials access-token) : Access a server's
  protected resource url with the given client credentials and an access
  token.

## Helpers

- (**oauth-parse-www-form-urlencoded** str #:optional charset) : Parses the
  given *str* string of name/value pairs as defined by the content type
  application/x-www-form-urlencoded and returns and association list. The keys
  and values in the association list are strings.

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


## Example: Twitter client

The following example details how to obtain the tweets of your Twitter home
timeline. The complete example is available as a web application under the
*examples* directory.

- Load the OAuth module:

```
> (use-modules (oauth oauth1))
```

- Define our Twitter URLs and application credentials:

```
> (define request-url "https://api.twitter.com/oauth/request_token")
> (define authorize-url "https://api.twitter.com/oauth/authorize")
> (define access-url "https://api.twitter.com/oauth/access_token")
> (define home-timeline "https://api.twitter.com/1.1/statuses/home_timeline.json")
> (define credentials (make-oauth1-credentials "key" "secret"))
```

  The *key* and *secret* are provided by Twitter once you register a new
  application at https://developer.twitter.com.

- Obtain a request token:

```
> (define request-token (oauth1-client-request-token request-url credentials))
```

- Connect to the following returned URL for authorizing the request token:

```
> (oauth1-client-authorize-url authorize-url request-token)
```

  Here you will need to login to Twitter or simply authorize your
  application if you are already logged in.

- Obtain the access token that will allow us to access protected resources:

```
> (define access-token
    (oauth1-client-access-token access-url credentials request-token "verifier"))
```

  The *verifier* is the string given by Twitter in the previous step.

- Get your tweets:

```
> (oauth1-client-request home-timeline credentials access-token)
```

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