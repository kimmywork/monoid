# Gamer's Witch Video Processing Pipeline

## Tools setup

 - ffmpeg
 - [youtube-dl](http://ytdl-org.github.io/youtube-dl/)
 - ["Get cookies.txt" extension](https://chrome.google.com/webstore/detail/get-cookiestxt/bgaddhkoddajcdgocldbbfleckgcbcid)


## Pipeline

 - Setup website profile, logging in and fetch cookie using the extension above,
 - Find specific videos and using youtube-dl download the video,
 - Perform any other steps using ffmpeg to optimize the video,
 - Upload and publish to related platforms (wechat videos for example).

## Examples

```sh
# weibo.cookie is the file contains cookie exported from chrome.
youtube-dl https://weibo.com/1597316331/MelYkrR7O --cookie weibo.cookie
```

## Automation

## Other tools to explore

 - [streamlink](https://streamlink.github.io/) example [here](https://github.com/jonghwanhyeon/python-ffmpeg/blob/main/examples/stdin-to-file.py)

