## Respond to `Range` requests:
    - Status code 206
    - EITHER:
        - Must include [Content-Range](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Range)
            - Problem: Full size not known; from-to/* possible but only one part
        - `Content-Type: "multipart/byterange"`
            