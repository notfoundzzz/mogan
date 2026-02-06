# PR access attempt: https://github.com/MoganLab/mogan/pull/2672

Attempted to fetch headers via curl. The request failed with a 403 (CONNECT tunnel failed).

```
$ curl -I -L https://github.com/MoganLab/mogan/pull/2672 | head -n 20
curl: (56) CONNECT tunnel failed, response 403
HTTP/1.1 403 Forbidden
content-length: 9
content-type: text/plain
date: Wed, 28 Jan 2026 05:41:47 GMT
server: envoy
connection: close
```
