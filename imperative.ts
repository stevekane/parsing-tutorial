const TRUE = 'true'
const FALSE = 'false'

enum TokenContext {
  BASE,
  STRING,
  NUMBER,
  BOOLEAN 
}
enum ParseContext {
  BASE,
  OBJ_KEY,
  OBJ_COLON,
  OBJ_VALUE,
  IN_ARRAY
}
enum TokenType {
  LBRACKET,
  RBRACKET,
  LSQBRACKET,
  RSQBRACKET,
  COMMA,
  COLON,
  STRING,
  NUMBER,
  BOOLEAN
}

type Obj<T> = { [ x: string ]: T }
type ParseResult<T> = [ number, T ] | Error
type Token
  = { type: TokenType.LBRACKET }
  | { type: TokenType.RBRACKET }
  | { type: TokenType.LSQBRACKET }
  | { type: TokenType.RSQBRACKET }
  | { type: TokenType.COMMA }
  | { type: TokenType.COLON }
  | { type: TokenType.STRING, value: string }
  | { type: TokenType.NUMBER , value: number }
  | { type: TokenType.BOOLEAN , value: boolean }

export function parse ( ts: Token[] ): ParseResult<any> {
  const result = parseAny(0, ts)

  return result instanceof Error ? result : result[1]
}

export function lex ( s: string ): Token[] | Error {
  var i = 0
  var tokens: Token[] = []
  var ctx: TokenContext = TokenContext.BASE
  var bfr = ''
  var c

  while ( c = s[i++] ) {
    switch ( ctx ) {
      case TokenContext.BASE: {
        bfr = ''
        switch ( c ) {
          case '{': 
            tokens.push({ type: TokenType.LBRACKET })
            break
          case '}': 
            tokens.push({ type: TokenType.RBRACKET })
            break
          case '[': 
            tokens.push({ type: TokenType.LSQBRACKET })
            break
          case ']': 
            tokens.push({ type: TokenType.RSQBRACKET })
            break
          case ',': 
            tokens.push({ type: TokenType.COMMA })
            break
          case ':': 
            tokens.push({ type: TokenType.COLON })
            break
          case '1': 
          case '2': 
          case '3': 
          case '4': 
          case '5': 
          case '6': 
          case '7': 
          case '8': 
          case '9': 
          case '0': 
            ctx = TokenContext.NUMBER
            bfr = c
            break
          case 't':
          case 'f':
            ctx = TokenContext.BOOLEAN
            bfr = c
            break
          case '"': 
            ctx = TokenContext.STRING
            break
        }
        break
      } 
      case TokenContext.STRING: {
        switch ( c ) {
          case '"':
            tokens.push({ type: TokenType.STRING, value: bfr })
            ctx = TokenContext.BASE
            break
          default:
            bfr += c
            break
        }
        break 
      }
      case TokenContext.NUMBER: {
        switch ( c ) {
          case '1': 
          case '2': 
          case '3': 
          case '4': 
          case '5': 
          case '6': 
          case '7': 
          case '8': 
          case '9': 
          case '0': 
            bfr += c
            break
          case '.':
            if ( bfr.indexOf('.') === -1 ) {
              bfr += '.'
            }
            else {                          
              return new Error(`Found extra . when parsing number`)
            }
            break
          case ',':
            tokens.push({ type: TokenType.NUMBER, value: Number(bfr) })
            tokens.push({ type: TokenType.COMMA })
            ctx = TokenContext.BASE
            break
          case ' ':
            tokens.push({ type: TokenType.NUMBER, value: Number(bfr) })
            ctx = TokenContext.BASE
            break
          default:
            return new Error(`Found ${ c } when parsing number`)
        }
        break 
      }
      case TokenContext.BOOLEAN: {
        bfr += c
        if ( bfr === TRUE ) {
          tokens.push({ type: TokenType.BOOLEAN, value: true })
          ctx = TokenContext.BASE
        }
        else if ( bfr === FALSE ) {
          tokens.push({ type: TokenType.BOOLEAN, value: false })
          ctx = TokenContext.BASE
        }
        else if ( TRUE.indexOf(bfr) === 0 || FALSE.indexOf(bfr) === 0 ) {
          continue
        }
        else {
          return new Error(`Found ${ c } when parsing boolean`)
        }
        break
      }
      default: const n: never = ctx
               return n
    }
  }
  return tokens
}

const test = 
`
{
  "name": "meta-ball",
  "version": 2.3,
  "scripts": {
    "test": "node test",
    "build": "run buildification"
  },
  "hacked": false,
  "versions": [ "2.2.2", "3.3.3" ],
  "dependencies": [
    { "racket": "1.2.3" },
    { "request": "7.7.7" }
  ]
}
`

const tokens = lex(test)

if ( tokens instanceof Error ) console.log(tokens)
else                           console.log(pp(parse(tokens)))

function pp ( o: any ): string { 
  return o instanceof Error ? o.message : JSON.stringify(o, null, 2)
}

function parseAny ( i: number, ts: Token[] ): ParseResult<any> {
  const t = ts[i++]

  switch ( t.type ) {
    case TokenType.STRING:
    case TokenType.BOOLEAN:
    case TokenType.NUMBER:
      return [ i, t.value ]
    case TokenType.LBRACKET:
      return parseObject(i, ts)
    case TokenType.LSQBRACKET:
      return parseArray(i, ts)
    default:
      return new Error(`${ JSON.stringify(ts[i]) } found at ${ i - 1 }`)
  }
}

function parseArray ( i: number, ts: Token[] ): ParseResult<any> {
  var out: any[] = []
  var n: Token
  var res: ParseResult<any>

  while ( true ) {
    res = parseAny(i, ts)

    if ( res instanceof Error ) return res

    i = res[0]
    n = ts[i++]
    out.push(res[1])

    switch ( n.type ) {
      case TokenType.COMMA:
        continue
      case TokenType.RSQBRACKET:
        return [ i, out ]
      default:
        return new Error('Expected , or ] in array parsing')
    }
  }  
}

function parseObject ( i: number, ts: Token[] ): ParseResult<any> {
  var out: Obj<any> = {}
  var vResult: ParseResult<any>
  var k: Token
  var c: Token
  var v: Token
  var n: Token
  var key = ''
  var value: any

  while ( true ) {
    k = ts[i++]

    if ( k == null ) return new Error('No key found in object')

    c = ts[i++]

    if ( c == null ) return new Error('Expected : in object')

    if ( k.type === TokenType.STRING && c.type === TokenType.COLON ) {
      v = ts[i++]

      if ( v == null ) return new Error('No valid valud found')

      vResult = parseAny(i - 1, ts)

      if ( vResult instanceof Error ) return vResult
      
      i = vResult[0]
      n = ts[i++]
      key = k.value
      value = vResult[1]
      out[key] = value

      if ( n == null ) return new Error('No valid terminator found')

      switch ( n.type ) {
        case TokenType.COMMA:    
          continue
        case TokenType.RBRACKET: 
          return [ i, out ]
        default:         
          return new Error(`found ${ pp(n) } after ${ k.value }. Expected , or }`)
      }
    }
    else {
      return new Error('Invalid thing found in parsing...')
    }
  }
}
