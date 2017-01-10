const src = `{name:"Steve Kane",age:5,tested:false}`

function pp ( a: any ) {
  console.log(a instanceof Error ? a.message : JSON.stringify(a))
}

// Let's parse: {
function leftCurly ( s: string ): '{' | Error {
  return s[0] === '{' ? '{' : Error(`Expected ${ '{ '}, found ${ s[0] }`)
}

// Let's generalize this to accept the character we're looking for
function exactly ( c: string, s: string ): string | Error {
  return s[0] === c ? c : Error(`Expected ${ c }, found ${ s[0] }`)
}

pp(leftCurly(src))
pp(exactly('{', src))

// Let's parse the first two characters: {n
function firstTwo ( s: string ): string | Error {
  const fst = leftCurly(s) 

  if ( fst instanceof Error ) return fst

  const snd = exactly('n', s.slice(1))

  if ( snd instanceof Error ) return snd

  return fst + snd
}

pp(firstTwo(src))
pp(firstTwo('{d'))

// Let's pass a list of parsers to run in a certain order
type Result = string | Error
type Parser = ( s: string ) => Result

function sequence ( ps: Parser[], s: string ): Result {
  var out = ''
  var res: Result

  for ( var i = 0; i < ps.length; i++ ) {
    res = ps[i](s[i])
    if ( res instanceof Error ) return res
    else                        out += res
  }
  return out
}

// we can see that parsers as defined above must be functions
// expecting exactly 1 argument ( unary ).  Let's 
// re-write our definition for exactly to be a partially applied
// function that returns a function expecting only the input string

function p_exactly ( c: string ) {
  return function ( s: string ): Result {
    return s[0] === c ? c : Error(`Expected ${ c }, found ${ s[0] }`)
  }
}

pp(sequence([ p_exactly('{'), p_exactly('n') ], src))

// Let's also re-write sequence to work similarly.  Sequence should
// accept the parsers as an argument and return a function that
// accepts the input string

function p_sequence ( ps: Parser[] ) { 
  return function ( s: string ): Result {
    var out = ''
    var res: Result

    for ( var i = 0; i < ps.length; i++ ) {
      res = ps[i](s[i])
      if ( res instanceof Error ) return res
      else                        out += res
    }
    return out
  }
}

// What if we wanted to parse the following: |__|

const leftGoalPost = p_sequence([ 
  p_exactly('|'), 
  p_exactly('_') 
])
const rightGoalPost = p_sequence([ 
  p_exactly('_'), 
  p_exactly('|') 
])
const goalPosts1 = p_sequence([ 
  p_exactly('|'), 
  p_exactly('_'), 
  p_exactly('_'), 
  p_exactly('|') 
])
const goalPosts2 = p_sequence([ 
  leftGoalPost, 
  rightGoalPost 
])

pp(goalPosts1('|__|'))
// pp(goalPosts2('|__|')) // fails as explained below

// goalPosts1 works as expected, but perhaps surprisingly
// goalPosts2 does not.  It fails at run-time and complains
// that it couldn't find any more data in the input string.
// 
// It turns out, we have encoded a subtle assumption
// in the definition of p_sequence:  We assume that each parser
// returns exactly 1 character. This, however, is not generally
// going to be the case as we build up more complex parsers.
// 
// As such, we need to modify our Result definition to capture
// the content that a parser has parsed as well as what remains
// to be parsed.  We will need to pass the remaining content 
// along to subsequent parsers to solve our above problem:

type Parsed = string
type Rest = string
type Res = [ Parsed, Rest ] | Error
type Prsr = ( s: Rest ) => Res

function e ( c: string ) {
  return function ( s: string ): Res {
    const h = s[0]
    const r = s.slice(1)

    return c == h ? [ c, r ] : new Error(`Expected ${ c }, found ${ h }`)
  }
}

function seq ( ps: Prsr[] ) {
  return function ( s: string ): Res {
    var out = ''
    var rest = s
    var res: Res

    for ( var i = 0; i < ps.length; i++ ) {
      res = ps[i](rest) 
      if ( res instanceof Error ) return res
      out += res[0]
      rest = res[1]
    }
    return [ out, rest ]
  }
}

const leftGP = seq([ 
  e('|'), 
  e('_') 
])
const rightGP = seq([ 
  e('_'), 
  e('|') 
])
const goalPosts3 = seq([ 
  leftGP, 
  rightGP 
])

pp(goalPosts3('|__|'))

// seq is nice when our parsers are a bit more general but
// it's awfully verbose for the case where we are parsing 
// for exactly some sequence of characters.  Let's write a
// parser that is more succinct for this common case

function match1 ( t: string ): Prsr {
  return seq(t.split('').map(e))
}

const goalPosts4 = match1('|__|')

pp(goalPosts4('|__|'))

// match1 works perfectly and is certainly more succinct for 
// the use case of matching an exact sequence of characters.
// However, the implementation requires several array allocations
// and is generally much slower than it needs to be.  Let's 
// write a new version of match that is more efficient.

function match ( t: string ) {
  return function ( s: string ): Res {
    var matched = true

    for ( var i = 0; i < t.length; i++ ) {
      matched = matched && t[i] == s[i] 
    }
    return matched 
      ? [ t, s.slice(t.length) ] 
      : new Error(`Expected ${ t }, found ${ s.slice(0, t.length) }`)
  }
}

const goalPosts5 = match('|__|')

pp(goalPosts5('|__|'))
pp(goalPosts5('|_|'))

// Let's try to write a more flexible parser that can accept characters
// satisfying a predicate function ( string ) => boolean

type Predicate<T> = ( t: T ) => boolean

function split ( i: number, s: string ): [ string, string ] {
  return [ s.slice(0, i), s.slice(i) ]
}

function satisfy ( p: Predicate<string> ) {
  return function ( s: string ): Res {
    return p(s[0]) ? split(1, s) : new Error(`Expected ${ p }, found ${ s[0] }`)
  }
}

function isAlpha ( s: string ): boolean {
  var cc = s.charCodeAt(0)

  return !isNaN(cc) && (( cc >= 65 && cc <= 90 ) || ( cc >= 97 && cc <= 122 ))
}

function isNumber ( s: string ): boolean {
  var cc = s.charCodeAt(0)

  return !isNaN(cc) && cc >= 48 && cc <= 57
}

const alpha = satisfy(isAlpha)
const num = satisfy(isNumber)

pp(alpha('abc'))
pp(alpha('1'))
pp(num('123'))

// Let's write a parser that consumes 0-N characters that satisfy a predicate

function consume ( p: Predicate<string> ) {
  return function ( s: string ): Res {
    var i = -1

    while ( s[++i] && p(s[i]) ) {}
    return split(i, s)
  }
}

const alphas = consume(isAlpha)

pp(alphas('abc123'))
pp(alphas('abcd'))

// Let's use the new kind of parser we have made to parse comments from javascript!

const comment = seq([
  match('//'),
  consume(c => c[0] != '\n')
])

const code ='// comments for your benefit\nvar foo = 1'

pp(comment(code))

// This is going really well!  Let's write another parser using the tools we have 
// now that will parse the second line of that code block.  We will need to parse:
// 'var', spaces, an identifier, spaces, '=', spaces, numbers
// Please note, this is still a naive parser as far as robustly parsing javascript,
// but this will illustrate the capabilities of our system thus far.

// N.B. we will need to remember to consume the linebreaks as well!

const isAlphaNum = ( s: string ) => isAlpha(s) || isNumber(s)
const spaces = consume(s => s[0] == ' ')
const identifier = seq([ satisfy(isAlpha), consume(isAlphaNum) ])
const linebreak = e('\n')
const assignmentStatement = seq([
  match('var'), 
  spaces, 
  identifier, 
  spaces, 
  e('='), 
  spaces,
  consume(isNumber)
])
const parseCode = seq([   
  comment, 
  e('\n'), 
  assignmentStatement, 
])

pp(parseCode(code))

// This is working well in terms of correctly parsing a variety of character
// sequences.  However, one thing you may be noticing is that the parsed output
// is still just a string which is honestly not very useful if you're trying to
// parse the input string to produce some sort of structured data ( for example,
// parsing javascript into an abstract syntax tree or JSON into some Javascript
// objects.
//
// Let's write a variation of the sequence function that returns each parsed result
// as a separate string in an array.  This should help us access each parsed output
// independently as we take the parsed output and produce some output structure.

type Rslt<T> = [ T, string ] | Error

function list ( ps: Prsr[] ) {
  return function ( s: string ): Rslt<string[]> {
    var out: string[] = []
    var rest = s
    var res: Res

    for ( var i = 0; i < ps.length; i++ ) {
      res = ps[i](rest) 
      if ( res instanceof Error ) return res
      out.push(res[0])
      rest = res[1]
    }
    return [ out, rest ]
  }
}

const code2 = 'var bar = 123'
const statement = list([
  match('var'),
  spaces, 
  identifier, 
  spaces, 
  e('='), 
  spaces,
  consume(isNumber)
])

pp(statement(code2))

// Our output now looks like [ 'var ', ' ', 'bar', ' ', '=', ' ', '123' ]
// This is better but we can now see some new issues with this approach:
// We often need to parse certain punctuation or characters which have 
// meaning in the input format but no bearing on the output structure.
// An example of this would be the equals sign in the assignment statement.
// Some other characters like the spaces/newlines are important for
// the language we are parsing but do not actually need to be returned to
// us for the final result.  As such, it would be nice to write the sort
// of parsers that check for the expected character(s) but do not
// return them as part of the parsed result.

// We COULD write an instance of every parser that either does or does
// not return its output but that doesn't strike me as a very good solution.
// Let's instead try to define a structure that represents a generic parser
// and see if we can still achieve the same level of composition as we had
// with functions above.

type IResult<T>
  = { parsed: T, rest: string }
  | Error

type IParser<T> = {
  consume: boolean
  run(s:string): IResult<T>
}

function run<T> ( p: IParser<T>, s: string ): IResult<T> {
  return p.run(s)
}

class Match {
  run(s: string) {
    return s.indexOf(this.t) === 0
      ? { parsed: this.consume ? s.slice(0, this.t.length) : '', rest: s.slice(this.t.length) }
      : new Error(`Expected ${ this.t }, found ${ s.slice(0, this.t.length) }`)
  }
  constructor( public consume: boolean, public t: string ) {}
}

pp(run(new Match(true, 'a'), 'abc'))
pp(run(new Match(false, 'a'), 'abc'))
