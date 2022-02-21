# Emacs config

## `org-roam` capture

We also provide a custom `.desktop` file which marks Emacs as supporting the `org-protocol://` protocol. Combined with
`org-roam` and the following bookmarklet, we can capture a page from Firefox.

```
javascript:location.href='org-protocol://roam-ref?template=r&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+ encodeURIComponent(window.getSelection())
```
