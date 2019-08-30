![NPeg](/doc/npeg.png)

# Cookbook

This document contains a random collection of snippets and fragments which
might serve as inspiration when developing your parsers.


## Find the port number for the http service

This little pattern searches for the `http` service line in `/etc/services`
and captures the port number.

```
let p = patt: @("http" * +Space * >+Digit)
echo p.matchFile("/etc/services").captures
```

## Comma separated list

Below is a common parser that matches a list of one or more elements
separated by commas:

```
let p = peg "list":
  list <- elem * *( comma * elem )
  comma <- ','
  elem <- >+Alpha

echo p.match("one,two,three").captures()
```


## Parsing integers

This maches a signed integer and returns the parsed number as a nim `int `type.
For educational purposes only, you really want to use Nim's `parseInt()`
instead.

```
type IntParser = object
  val: int
  sign: int

let p = peg(IntParser, "int"):

  pre <- 0:
    userdata.val = 0
    userdata.sign = 1
  
  plus <- '+'
  
  minus <- '-':
    userdata.sign = -1
  
  sign <- plus | minus

  digit <- >{'0'..'9'}:
    let v = ord(($1)[0]) - ord('0')
    userdata.val = userdata.val * 10 + v

  post <- 0:
    userdata.val *= userdata.sign
  
  int <- pre * ?sign * +digit * post
```


## RTSP Parser

Very minwimal example. Note the use of `parseEnum()` to convert
a matched choice into a nim enum.

```
type

  RtspMethod = enum DESCRIBE, OPTIONS
  
  Rtsp = object
    meth: RtspMethod
    url: string
    cseq: int
    headers: Table[string, string]

let p = peg(Rtsp, "rtsp"):
  sp <- " "
  crlf <- "\r\n" | "\n"
  request <- meth * sp * url * sp * "RTSP/1.0" * crlf

  meth <- >("OPTIONS" | "DESCRIBE" | "SETUP" | "PLAY" | "TEARDOWN"):
    userdata.meth = parseEnum[RtspMethod]($1)

  headers <- *header
  header <- >(crlf | hdr_cseq | hdr_other) * crlf

  hdr_cseq <- i"CSeq:" * sp * >+Digit:
    userdata.cseq = parseInt($1)

  hdr_other <- >+(1-':') * ":" * sp * >*(1-'\n'):
    userdata.headers.add($1, $2)

  rtsp <- request * headers * !1
  url <- >+Graph:
    userdata.url = $1
```


## Here-documents

Matches posix-shell like here-documents using back references.

```
let p = peg "doc":
  S <- *Space
  doc <- +word * "<<" * R("sep", sep) * S * >heredoc * R("sep") * S * +word
  word <- +Alpha * S
  sep <- +Alpha
  heredoc <- *Alpha * +(1 - R("sep"))

let d = """This is a <<EOT here document
with multiple lines EOT end"""

let r = p.match(d)
doAssert r.ok
echo r.captures[0]
```

## Git logs

This parser matches the output of `git log`:

```
var commits = 0

let p = peg "log":
  nl <- "\n"
  log <- +entry
  entry <- header * body
  header <- commit * +info
  commit <- "commit " * >+Xdigit * nl:
    inc commits
    echo $1
  info <- info_author | info_other
  info_author <- "Author: " * >+(1-" <") * " <" * >+(1-'>') * '>' * nl:
    echo $1, " ", $1
    discard
  info_other <- >+Alpha * ":" * +Blank * >+(1-nl) * nl:
    echo " ", $1, ": ", $2
    discard
  bodyline <- !"commit" * *(1-nl) * nl
  body <- +bodyline

let d = readfile "/tmp/log"
echo p.match(d)
echo commits
```


## E-mail addresses

Matching e-mail addresses according to the RFC spec is horrific:

```
let matcher = peg "mail":
  WSP <- {' ', '\t'}
  CRLF <- "\r\l"
  obs_FWS <- +WSP * *(CRLF * +WSP)
  FWS <- (?(*WSP * CRLF) * +WSP) | obs_FWS
  obs_ctext <- {'\1'..'\8'} | '\11' | '\12' | {'\14'..'\31'} | '\127'
  obs_qp <- '\\' * ('\0' | obs_ctext | '\l' | '\r')
  quoted_pair <- ('\\' * ({'\x21'..'\x7E'} | WSP)) | obs_qp

  ctext <- {'\33'..'\39'} | {'\42'..'\91'} | {'\93'..'\126'} | obs_ctext

  ccontent <- ctext | quoted_pair | comment
  comment <- '(' * *(?FWS * ccontent) * ?FWS * ')'
  CFWS <- (+(?FWS * comment) * ?FWS) | FWS

  atext <- Alpha | Digit | '!' | '#' | '$' | '%' | '&' | '\'' | '*' |
           '+' | '-' | '/' | '=' | '?' | '^' | '_' | '`' | '{' | '|' |
           '}' | '~'
  atom <- ?CFWS * +atext * ?CFWS
  dot_atom_text <- +atext * *('.' * +atext)
  dot_atom <- ?CFWS * dot_atom_text * ?CFWS

  qtext <- '\33' | {'\35'..'\91'} | {'\93'..'\126'} | obs_ctext
  qcontent <- qtext | quoted_pair
  quoted_string <- ?CFWS * '"' * *(?FWS * qcontent) * ?FWS * '"' * ?CFWS

  word <- atom | quoted_string

  obs_local_part <- word * *('.' * word)
  local_part <- dot_atom | quoted_string | obs_local_part

  obs_dtext <- obs_ctext | quoted_pair
  dtext <- {'\33'..'\90'} | {'\94'..'\126'} | obs_dtext
  domain_literal <- ?CFWS * '[' * *(?FWS * dtext) * ?FWS * ']' * ?CFWS
  obs_domain <- atom * *('.' * atom)
  domain <- dot_atom | domain_literal | obs_domain

  ipv4 <- Snum * ('.' * Snum)[3]
  Snum <- Digit[1..3]
  ipv6_full <- ipv6_hex * (':' * ipv6_hex)[7]
  ipv6_comp <- ipv6_comp_a * "::" * ipv6_comp_a
  ipv6_comp_a <- ?(ipv6_hex * *((':' * ipv6_hex)[5])) 
  ipv6v4_full <- ipv6_hex * (':' * ipv6_hex)[5] * ':' * ipv4
  ipv6v4_comp <- ?(ipv6_hex * *((':' * ipv6_hex)[3])) * "::" *
                 ?(ipv6_hex * *((':' * ipv6_hex)[3]) * ':') * ipv4
  ipv6_hex <- Xdigit[1..4]
  ipv6_addr <- ipv6_full | ipv6_comp | ipv6v4_full | ipv6v4_comp
  ipv6 <- i"IPv6:" * ipv6_addr
  dcontent <- {'\33'..'\90'} | {'\94'..'\126'}
  general_addr <- *(Alpha | Digit | '-') * (Alpha | Digit) * ':' * +dcontent
  address_literal <- '[' * (ipv4 | ipv6 | general_addr) * ']'
  mail <- local_part * '@' * (domain | address_literal)

echo matcher.match("hello@nim-lang.org").ok
```


## Advent of code

This one is silly, this matches the input of the Advent of Code 2018,
exercise #20 (https://adventofcode.com/2018/day/20):

```
var x, y: int
var stack: seq[tuple[x: int, y: int]]

let p = peg "walk":
  walk <- "^" * dirs * "$"
  n <- 'N': dec y
  e <- 'E': inc x
  w <- 'W': dec x
  s <- 'S': inc y
  open <- '(': stack.add (x, y)
  close <- ')': stack.delete stack.high
  pipe <- '|': stack[stack.high] = (x, y)
  nesw <- n | e | w | s:
    stdout.write "\e[" & $y & ";" & $x & "H*"
  dir <- *(nesw | par)
  dirs <- dir * *( pipe * dir)
  par <- open * dirs * close

stdout.write "\e[2J"
let r = p.match("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
```
