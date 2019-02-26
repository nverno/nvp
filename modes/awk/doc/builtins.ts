// https://github.com/theovosse/awk-language-server/blob/master/server/src/awk.ll
/**
 * Description of built-in functions and variables for hover
 */
export interface BuiltInFunction {
    name: string;
    parameters?: string[];
    firstOptional?: number;
    maxNrArguments?: number;
    returns?: string;
    awk: boolean;
    description: string;
}

/**
 * All known built-in functions and variables
 */
export let builtInSymbols: { [name: string]: BuiltInFunction } = {
    atan2: {
        name: "atan2",
        parameters: ["y", " x"],
        description: "Return the arctangent of y / x in radians. You can use 'pi = atan2(0, -1)' to retrieve the value of pi.",
        awk: false
    },
    cos: {
        name: "cos",
        parameters: ["x"],
        description: "Return the cosine of x, with x in radians.",
        awk: false
    },
    exp: {
        name: "exp",
        parameters: ["x"],
        description: "Return the exponential of x (e ^ x) or report an error if x is out of range. The range of values x can have depends on your machine's floating-point representation.",
        awk: false
    },
    int: {
        name: "int",
        parameters: ["x"],
        description: "Return the nearest integer to x, located between x and zero and truncated toward zero. For example, int(3) is 3, int(3.9) is 3, int(-3.9) is -3, and int(-3) is -3 as well.",
        awk: true
    },
    log: {
        name: "log",
        parameters: ["x"],
        description: "Return the natural logarithm of x, if x is positive; otherwise, return NaN (\"not a number\") on IEEE 754 systems. Additionally, gawk prints a warning message when x is negative.",
        awk: false
    },
    rand: {
        name: "rand",
        parameters: [],
        description: "Return a random number. The values of rand() are uniformly distributed between zero and one. The value could be zero but is never one.43",
        awk: true
    },
    sin: {
        name: "sin",
        parameters: ["x"],
        description: "Return the sine of x, with x in radians.",
        awk: true
    },
    sqrt: {
        name: "sqrt",
        parameters: ["x"],
        description: "Return the positive square root of x. gawk prints a warning message if x is negative. Thus, sqrt(4) is 2.",
        awk: false
    },
    srand: {
        name: "srand",
        parameters: ["x"],
        firstOptional: 0,
        description: "Set the starting point, or seed, for generating random numbers to the value x.",
        awk: false
    },
    asort: {
        name: "asort",
        parameters: ["source", " dest", " how"],
        firstOptional: 1,
        description: "Return the number of elements in the array source. For asort(), gawk sorts the values of source and replaces the indices of the sorted values of source with sequential integers starting with one. If the optional array dest is specified, then source is duplicated into dest. dest is then sorted, leaving the indices of source unchanged.",
        awk: false
    },
    asorti: {
        name: "asorti",
        parameters: ["source", " dest", " how"],
        firstOptional: 1,
        description: "Return the number of elements in the array source. For asort(), gawk sorts the values of source and replaces the indices of the sorted values of source with sequential integers starting with one. If the optional array dest is specified, then source is duplicated into dest. dest is then sorted, leaving the indices of source unchanged.",
        awk: false
    },
    gensub: {
        name: "gensub",
        parameters: ["regexp", " replacement", " how", " target"],
        firstOptional: 3,
        description: "Search the target string target for matches of the regular expression regexp. If how is a string beginning with 'g' or 'G' (short for \"global\"), then replace all matches of regexp with replacement. Otherwise, how is treated as a number indicating which match of regexp to replace. If no target is supplied, use $0. It returns the modified string as the result of the function and the original target string is not changed. gensub() provides an additional feature that is not available in sub() or gsub(): the ability to specify components of a regexp in the replacement text. This is done by using parentheses in the regexp to mark the components and then specifying '\\N' in the replacement text, where N is a digit from 1 to 9",
        awk: false
    },
    getline: {
        name: "getline",
        parameters: [],
        firstOptional: 0,
        description: "Get line from input",
        awk: true
    },
    gsub: {
        name: "gsub",
        parameters: ["regexp", " replacement", " target"],
        firstOptional: 2,
        description: "Search target for all of the longest, leftmost, nonoverlapping matching substrings it can find and replace them with replacement. The 'g' in gsub() stands for \"global,\" which means replace everywhere. For example:",
        awk: true
    },
    index: {
        name: "index",
        parameters: ["in", " find"],
        description: "Search the string in for the first occurrence of the string find, and return the position in characters where that occurrence begins in the string in. Consider the following example:",
        awk: true
    },
    length: {
        name: "length",
        parameters: ["string"],
        firstOptional: 0,
        description: "Return the number of characters in string. If string is a number, the length of the digit string representing that number is returned. For example, length(\"abcde\") is five. If no argument is supplied, length() returns the length of $0.",
        awk: true
    },
    match: {
        name: "match",
        parameters: ["string", " regexp", " array"],
        firstOptional: 2,
        description: "Search string for the longest, leftmost substring matched by the regular expression regexp and return the character position (index) at which that substring begins (one, if it starts at the beginning of string). If no match is found, return zero.",
        awk: true
    },
    patsplit: {
        name: "patsplit",
        parameters: ["string", " array", " fieldpat", " seps"],
        firstOptional: 2,
        description: "Divide string into pieces defined by fieldpat and store the pieces in array and the separator strings in the seps array. The first piece is stored in array[1], the second piece in array[2], and so forth. The third argument, fieldpat, is a regexp describing the fields in string (just as FPAT is a regexp describing the fields in input records). It may be either a regexp constant or a string. If fieldpat is omitted, the value of FPAT is used. patsplit() returns the number of elements created. seps[i] is the separator string between array[i] and array[i+1]. Any leading separator will be in seps[0].",
        awk: false
    },
    split: {
        name: "split",
        parameters: ["string", " array", " fieldsep", " seps"],
        firstOptional: 2,
        description: "Divide string into pieces separated by fieldsep and store the pieces in array and the separator strings in the seps array. The first piece is stored in array[1], the second piece in array[2], and so forth. The string value of the third argument, fieldsep, is a regexp describing where to split string (much as FS can be a regexp describing where to split input records). If fieldsep is omitted, the value of FS is used. split() returns the number of elements created. seps is a gawk extension, with seps[i] being the separator string between array[i] and array[i+1]. If fieldsep is a single space, then any leading whitespace goes into seps[0] and any trailing whitespace goes into seps[n], where n is the return value of split() (i.e., the number of elements in array).",
        awk: true
    },
    print: {
        name: "print",
        parameters: ["expression"],
        firstOptional: 0,
        maxNrArguments: Number.MAX_SAFE_INTEGER,
        description: "Print the expression, or $0 if missing.",
        awk: true
    },
    printf: {
        name: "printf",
        firstOptional: 1,
        maxNrArguments: Number.MAX_SAFE_INTEGER,
        parameters: ["format", " expression1", " ..."],
        description: "Print the format string replacing the arguments with the expressions.",
        awk: true
    },
    sprintf: {
        name: "sprintf",
        firstOptional: 1,
        maxNrArguments: Number.MAX_SAFE_INTEGER,
        parameters: ["format", " expression1", " ..."],
        description: "Return (without printing) the string that printf would have printed out with the same arguments (see Printf).",
        awk: true
    },
    strtonum: {
        name: "strtonum",
        parameters: ["str"],
        description: "Examine str and return its numeric value. If str begins with a leading '0', strtonum() assumes that str is an octal number. If str begins with a leading '0x' or '0X', strtonum() assumes that str is a hexadecimal number. For example:",
        awk: false
    },
    sub: {
        name: "sub",
        parameters: ["regexp", " replacement", " target"],
        firstOptional: 2,
        description: "Search target, which is treated as a string, for the leftmost, longest substring matched by the regular expression regexp. Modify the entire string by replacing the matched text with replacement. The modified string becomes the new value of target. Return the number of substitutions made (zero or one).",
        awk: true
    },
    substr: {
        name: "substr",
        parameters: ["string", " start", " length"],
        firstOptional: 2,
        description: "Return a length-character-long substring of string, starting at character number start. The first character of a string is character number one.",
        awk: true
    },
    tolower: {
        name: "tolower",
        parameters: ["string"],
        description: "Return a copy of string, with each uppercase character in the string replaced with its corresponding lowercase character. Nonalphabetic characters are left unchanged.",
        awk: true
    },
    toupper: {
        name: "toupper",
        parameters: ["string"],
        description: "Return a copy of string, with each lowercase character in the string replaced with its corresponding uppercase character. Nonalphabetic characters are left unchanged.",
        awk: true
    },
    close: {
        name: "close",
        parameters: ["filename", " how"],
        firstOptional: 1,
        description: "Close the file filename for input or output. Alternatively, the argument may be a shell command that was used for creating a coprocess, or for redirecting to or from a pipe; then the coprocess or pipe is closed. See Close Files And Pipes for more information.",
        awk: false
    },
    fflush: {
        name: "fflush",
        parameters: ["filename"],
        firstOptional: 0,
        description: "Flush any buffered output associated with filename, which is either a file opened for writing or a shell command for redirecting output to a pipe or coprocess.",
        awk: false
    },
    system: {
        name: "system",
        parameters: ["command"],
        description: "Execute the operating system command command and then return to the awk program. Return command's exit status (see further on).",
        awk: true
    },
    exit: {
        name: "exit",
        parameters: ["status"],
        firstOptional: 0,
        description: "Exits the awk script immediately",
        awk: true
    },
    mktime: {
        name: "mktime",
        parameters: ["datespec"],
        description: "Turn datespec into a timestamp in the same form as is returned by systime(). The argument, datespec, is a string of the form \"YYYY MM DD HH MM SS [DST]\". The string consists of six or seven numbers representing, respectively, the full year including century, the month from 1 to 12, the day of the month from 1 to 31, the hour of the day from 0 to 23, the minute from 0 to 59, the second from 0 to 60,54 and an optional daylight-savings flag.",
        awk: false
    },
    systime: {
        name: "systime",
        parameters: [],
        description: "Return the current time as the number of seconds since the system epoch. On POSIX systems, this is the number of seconds since 1970-01-01 00:00:00 UTC, not counting leap seconds. It may be a different number on other systems. ",
        awk: false
    },
    strftime: {
        name: "strftime",
        parameters: ["format", "timestamp", "utc-flag"],
        description: "Format the time specified by timestamp based on the contents of the format string and return the result. It is similar to the function of the same name in ISO C. If utc-flag is present and is either nonzero or non-null, the value is formatted as UTC (Coordinated Universal Time, formerly GMT or Greenwich Mean Time). Otherwise, the value is formatted for the local time zone. The timestamp is in the same format as the value returned by the systime() function. If no timestamp argument is supplied, gawk uses the current time of day as the timestamp. Without a format argument, strftime() uses the value of PROCINFO[\"strftime\"] as the format string (see Built-in Variables). The default string value is \"%a %b %e %H:%M:%S %Z %Y\". This format string produces output that is equivalent to that of the date utility.",
        awk: false
    },
    and: {
        name: "and",
        parameters: ["v1", " v2", " ..."],
        firstOptional: 2,
        description: "Return the bitwise AND of the arguments. There must be at least two.",
        awk: false
    },
    compl: {
        name: "compl",
        parameters: ["val"],
        description: "Return the bitwise complement of val.",
        awk: false
    },
    lshift: {
        name: "lshift",
        parameters: ["val", " count"],
        description: "Return the value of val, shifted left by count bits.",
        awk: false
    },
    or: {
        name: "or",
        parameters: ["v1", " v2", " ..."],
        firstOptional: 2,
        description: "Return the bitwise OR of the arguments. There must be at least two.",
        awk: false
    },
    rshift: {
        name: "rshift",
        parameters: ["val", " count"],
        description: "Return the value of val, shifted right by count bits.",
        awk: false
    },
    xor: {
        name: "xor",
        parameters: ["v1", " v2", " ..."],
        firstOptional: 2,
        description: "Return the bitwise XOR of the arguments. There must be at least two. ",
        awk: false
    },
    isarray: {
        name: "isarray",
        parameters: ["x"],
        description: "Return a true value if x is an array. Otherwise, return false. ",
        awk: false
    },
    bindtextdomain: {
        name: "bindtextdomain",
        parameters: ["directory", " domain"],
        firstOptional: 1,
        description: "Set the directory in which gawk will look for message translation files, in case they will not or cannot be placed in the \"standard\" locations (e.g., during testing). It returns the directory in which domain is \"bound.\"",
        awk: false
    },
    dcgettext: {
        name: "dcgettext",
        parameters: ["string", " domain", " category"],
        firstOptional: 2,
        description: "Return the translation of string in text domain domain for locale category category. The default value for domain is the current value of TEXTDOMAIN. The default value for category is \"LC_MESSAGES\".",
        awk: false
    },
    dcngettext: {
        name: "dcngettext",
        parameters: ["string1", " string2", " number", " domain", " category"],
        firstOptional: 3,
        description: "Return the plural form used for number of the translation of string1 and string2 in text domain domain for locale category category. string1 is the English singular variant of a message, and string2 is the English plural variant of the same message. The default value for domain is the current value of TEXTDOMAIN. The default value for category is \"LC_MESSAGES\".",
        awk: false
    },
    ARGC: {
        name: "ARGC",
        awk: true,
        description: "The number of command  line  arguments  (does  not  include options to gawk, or the program source)."
    },
    ARGIND: {
        name: "ARGIND",
        awk: true,
        description: "The index in ARGV of the current file being processed."
    },
    ARGV: {
        name: "ARGV",
        awk: true,
        description: "Array of command line arguments.  The array is indexed from 0 to ARGC - 1.  Dynamically changing the contents  of  ARGV can control the files used for data."
    },
    BINMODE: {
        name: "BINMODE",
        awk: false,
        description: "On  non-POSIX  systems,  specifies use of \"binary\" mode for all file I/O.  Numeric values of 1, 2, or 3,  specify  that input  files,  output  files,  or  all files, respectively, should use binary I/O.  String values of \"r\", or \"w\"  specify that input files, or output files, respectively, should use binary I/O.  String values of \"rw\" or \"wr\" specify that all files should use binary I/O.  Any other string value is treated as \"rw\", but generates a warning message."
    },
    CONVFMT: {
        name: "CONVFMT",
        awk: false,
        description: "The conversion format for numbers, \"%.6g\", by default."
    },
    ENVIRON: {
        name: "ENVIRON",
        awk: true,
        description: "An array containing the values of the current  environment. The  array  is  indexed  by the environment variables, each element being the  value  of  that  variable  (e.g.,  ENVIRON[\"HOME\"]  might be \"/home/arnold\").  Changing this array does not affect the environment seen by programs which gawk spawns via redirection or the system() function."
    },
    ERRNO: {
        name: "ERRNO",
        awk: false,
        description: "If  a  system  error  occurs either doing a redirection for getline, during a read for getline, or  during  a  close(), then ERRNO will contain a string describing the error.  The value is subject to translation in non-English locales."
    },
    FIELDWIDTHS: {
        name: "FIELDWIDTHS",
        awk: false,
        description: "A whitespace separated list of  field  widths.   When  set, gawk  parses  the input into fields of fixed width, instead of using the value of the FS variable as the field  separator.  See Fields, above."
    },
    FILENAME: {
        name: "FILENAME",
        awk: true,
        description: "The name of the current input file.  If no files are specified on the command line, the value  of  FILENAME  is  \"-\". However,  FILENAME  is  undefined  inside  the  BEGIN  rule (unless set by getline)."
    },
    FNR: {
        name: "FNR",
        awk: true,
        description: "The input record number in the current input file."
    },
    FPAT: {
        name: "FPAT",
        awk: false,
        description: "A regular expression describing the contents of the  fields in  a record.  When set, gawk parses the input into fields, where the fields match the regular expression,  instead  of using  the value of the FS variable as the field separator. See Fields, above."
    },
    FS: {
        name: "FS",
        awk: true,
        description: "The input field separator, a space by default.  See Fields, above."
    },
    FUNCTAB: {
        name: "FUNCTAB",
        awk: false,
        description: "An  array  whose  indices  and corresponding values are the names of all the user-defined or extension functions in the program.   NOTE:  You may not use the delete statement with the FUNCTAB array."
    },
    IGNORECASE: {
        name: "IGNORECASE",
        awk: false,
        description: "Controls the case-sensitivity of all regular expression and string  operations.   If  IGNORECASE  has a non-zero value, then string comparisons  and  pattern  matching  in  rules, field  splitting  with  FS and FPAT, record separating with RS, regular expression matching with ~ and !~, and the gensub(),  gsub(),  index(), match(), patsplit(), split(), and sub() built-in functions all ignore case when doing regular expression  operations.   NOTE:  Array  subscripting is not affected.  However, the asort() and asorti() functions  are affected. Thus,  if IGNORECASE is not equal to zero, /aB/ matches all of the strings \"ab\", \"aB\", \"Ab\", and \"AB\".  As with all AWK variables,  the initial value of IGNORECASE is zero, so all regular expression and string operations are normally case-sensitive."
    },
    LINT: {
        name: "LINT",
        awk: false,
        description: "Provides  dynamic  control of the --lint option from within an AWK program.  When true, gawk prints lint warnings. When false,  it  does  not.   When  assigned  the  string  value \"fatal\", lint warnings become fatal  errors,  exactly  like --lint=fatal.  Any other true value just prints warnings."
    },
    NF: {
        name: "NF",
        awk: true,
        description: "The number of fields in the current input record."
    },
    NR: {
        name: "NR",
        awk: true,
        description: "The total number of input records seen so far."
    },
    OFMT: {
        name: "OFMT",
        awk: false,
        description: "The output format for numbers, \"%.6g\", by default."
    },
    OFS: {
        name: "OFS",
        awk: true,
        description: "The output field separator, a space by default."
    },
    ORS: {
        name: "ORS",
        awk: false,
        description: "The output record separator, by default a newline."
    },
    PREC: {
        name: "PREC",
        awk: false,
        description: "The working precision of arbitrary precision floating-point numbers, 53 by default."
    },
    PROCINFO: {
        name: "PROCINFO",
        awk: false,
        description: "The elements of this array provide  access  to  information about  the running AWK program."
    },
    ROUNDMODE: {
        name: "ROUNDMODE",
        awk: false,
        description: "The rounding mode to use for arbitrary precision arithmetic on numbers, by default \"N\" (IEEE-754 roundTiesToEven mode). The accepted values are \"N\" or \"n\" for roundTiesToEven, \"U\" or \"u\" for roundTowardPositive, \"D\" or \"d\" for roundToward- Negative, \"Z\" or \"z\" for roundTowardZero, and if your  version  of  GNU  MPFR  library  supports  it,  \"A\" or \"a\" for roundTiesToAway."
    },
    RS: {
        name: "RS",
        awk: false,
        description: "The input record separator, by default a newline."
    },
    RT: {
        name: "RT",
        awk: false,
        description: "The record terminator.  Gawk sets RT to the input text that matched  the  character  or regular expression specified by RS."
    },
    RSTART: {
        name: "RSTART",
        awk: true,
        description: "The index of the first character matched by match();  0  if no  match.   (This  implies that character indices start at one.)"
    },
    RLENGTH: {
        name: "RLENGTH",
        awk: true,
        description: "The length of the string  matched  by  match();  -1  if  no match."
    },
    SUBSEP: {
        name: "SUBSEP",
        awk: true,
        description: "The character used to separate multiple subscripts in array elements, by default \"\\034\"."
    },
    SYMTAB: {
        name: "SYMTAB",
        awk: false,
        description: "An array whose indices  are  the  names  of  all  currently defined  global  variables  and arrays in the program."
    },
    TEXTDOMAIN: {
        name: "TEXTDOMAIN",
        awk: false,
        description: "The text domain of the AWK program; used to find the localized translations for the program's strings."
    }
}
