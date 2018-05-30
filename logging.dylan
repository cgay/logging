Module:    log
Author:    Carl L Gay
Synopsis:  Simple logging mechanism.  Some ideas taken from log4j.
Copyright: Copyright (c) 2013 Dylan Hackers.  See License.txt for details.

// This file has been converted to a hypothetical new module system
// where "define module" is replaced by import statements in the same
// file (or files) with the main code. "export" clauses are replaced
// by "public" adjectives on define forms. I want to get a sense of
// how this might feel.

// A big difference is that used modules may be referenced directly
// via "foo:bar" syntax. The compiler should be able to convert this
// new syntax into the equivalennt of importing "bar" from "foo" and
// then referencing it as "bar". For example:
//
//   use system.date;
//   use io.format;
//
//   format:to-stream("The time is now %s.", date:now());
//
// There is no conflict between the local variable named "date" and
// the module imported as "date" because module names have their own
// namespace and the foo:bar syntax unambiguously indicates a module
// reference. This can be used for documentation purposes, for example:
//
//   define method common-dylan:print-object ...
//
//
// A library is defined by the build system (e.g., a LID file) as a
// set of files and a set of exported modules. We could either specify
// the list of exported modules in the LID file, or we could use a
// naming convention, like %module to mean internal module.
//
// Any file may import or export modules and names. The full module
// definition is the sum of those imports and exports. Redundant
// imports (e.g., in different files) are acceptable. Redundant
// exports result in a warning.
//
// TODO: How do we test internals? Can we build the tests into the
//       module-under-test?  Probably.  Another possibility is to use
//       naming conventions: "public %foo" public, but is internal by
//       convention because of the leading %. This is a little worse
//       than exporting an internal-only module though, because it
//       requires adding a leading % to anything internal you want to
//       test. I don't like it, and I think we can use the build system.
//
// TODO: Do we need something to get the equivalent of what the
//       "create" clause provides now?

use dylan, import: *;  // Use { \* } to import the actual name "*".
use common-extensions, import: *;
use io.streams;

// Import two classes from the date module so they can be used without
// the module prefix. Note that any import from a module makes the
// module itself accessible via module:name syntax.
//
// Note: I made some name changes such as current-date -> date:now
// and format-date -> date:format, which reflect the way I think
// things would be named in a world where you can use module:
// qualifiers.
use system.date, import: { <date>, <duration> };

// Note that without "as fs", names would be accessible as "file-system:foo",
// not "system.file-system:foo".
use system.file-system as fs;

// Use the format module without any explicit imports.
// format-to-string becomes just to-string and is referenced as
// format:to-string. Similar for format:to-stream etc.
use format;

use generic-arithmetic, import: { <integer> as <double-integer>,
                                  \+ as plus,
                                  \* as mul,
                                  \/ as div };
use system.locators, import: { <locator>, <file-locator>, locator-name, merge-locators };


/* 

See README.rst for documentation.

todo -- implement keep-versions in <rolling-file-log-target>

todo -- implement compress-on-close? in <rolling-file-log-target>

todo -- configuration parser

todo -- more documentation

todo -- more tests

todo -- Handle errors gracefully.  e.g., if the disk fills up it may be
        better to do nothing than to err.  Catch errors in user code when
        logging a message and log "*** error generating log message ***",
        for example.  If logging to stream/file fails, log to stderr as
        a fallback.  (e.g., someone forks and closes all fds)

todo -- <file-log-target> should accept a string for the filename to
        avoid making people import locators.  God I hate the locators
        library.

todo -- <rolling-file-log-target>: Should roll the file when either a
        max size or a max time is reached, whichever comes first.
        Should make it possible for users to override roll-log-file?
        and rolled-log-file-name methods if they want to roll their
        own.  Should also have option to compress on roll.  Should also
        be able to specify that it roll "daily at midnight" etc.

todo -- Add a way to extend the set of format directives from outside
        the library.  Get rid of code duplication in formatter parsing.

??? -- Is there a reasonable use case where you might not want \n at the
       end of each log entry?  Rather than hard-coding the \n one could
       specify it in the formatter's control string.  The worry is that
       everyone would forget to add it every time they write a new formatter.

idea -- There's often a tension between the level of logging you want
        to retain permanently and the level of logging you need for
        debugging.  Could support writing different log levels to
        additional targets.  Then one could log debug messages to a
        separate file and set them to roll every 5 minutes, with a
        small number of revisions, and you have essentially a circular
        buffer of recent debug info.  Log to RAMdisk...even better, to
        avoid disk contention.  :-)

idea -- It is useful for general purpose libraries (e.g., an XML parser)
        to do logging.  You normally want this logging disabled.  A calling
        library will probably want to turn on the XML parser's logging for
        specific threads, for debugging purposes.  The XML parser can use
        an exported thread variable to hold its debug log and callers can
        rebind that to the log they want.  (Not really an issue for this
        logging library to address...more of a suggestion for something to
        add to future documentation.)  Just enabling the XML parser's log
        won't always be what users want because it will enable logging in
        all threads.

todo -- Look at concurrency issues.  For example, is it possible for log
        messages to be written with out-of-order timestamps when multiple
        threads log to the same file via different log targets.  Can either
        document that and say "don't do that" or fix it.  Similarly, could
        add OPTIONAL file locking (a la fcntl.flock(fd, LOCK_EX)) so that
        multiple processes can ensure that large log messages are written
        atomically and guarantee monotonically increasing log entry dates.
        Must be optional since it's heavyweight.

idea -- Support log categories.  Each log message is associated with a
        category.  Each category has a log level associated with it.  This
        makes it easy to adjust the types of debug logging per category at
        run time.  Categories could be hierarchical so that messages from
        entire subsystems can be enabled/disabled en masse.

todo -- See http://pypi.python.org/pypi/LogPy/1.0 for some (well, at least one)
        interesting ideas.  Attach arbitrary tags to log messages (instead of
        hierarchical categories or in addition to?).

*/


///////////////////////////////////////////////////////////
//// Log classes
////

define variable $root :: false-or(<log>) = #f;

define public sealed generic name
    (log :: <abstract-log>) => (name :: <string>);
define sealed generic parent
    (log :: <abstract-log>) => (parent :: false-or(<abstract-log>));
define sealed generic children
    (log :: <abstract-log>) => (children :: <string-table>);
define public sealed generic additive?
    (log :: <abstract-log>) => (additive? :: <boolean>);
define public sealed generic enabled?
    (log :: <abstract-log>) => (enabled? :: <boolean>);

define public abstract class <abstract-log> (<object>)
  // A dotted path name.  All parent logs in the path must already exist.
  constant slot name :: <string>,
    required-init-keyword: name:;

  slot parent :: false-or(<abstract-log>) = #f,
    init-keyword: parent:;

  constant slot children :: <string-table> = make(<string-table>),
    init-keyword: children:;

  // If this is #t then log messages sent to this log will be passed up
  // the hierarchy to parent logs as well, until it reaches a log
  // whose additivity is #f.  Terminology stolen from log4j.
  //
  slot additive? :: <boolean> = #t,
    init-keyword: additive?:;

  // If disabled, no messages will be logged to this log's targets.
  // The value of additive? will still be respected.  In other
  // words, logging to a disabled log will still log to ancestor
  // logs if they are themselves enabled.
  //
  slot enabled? :: <boolean> = #t,
    init-keyword: enabled?:;

end class <abstract-log>;

define method initialize
    (log :: <abstract-log>, #key name :: <string>)
  next-method();
  if ($root)
    // This call to "add" would be ambiguous. Could be dylan:add or
    // log:add, so the compiler should warn and choose the current
    // module. We add the current module prefix to prevent a warning.
    log:add($root, log, as(<list>, split(name, '.')), name);
  end;
end method initialize;

define function local-name
    (log :: <abstract-log>)
 => (local-name :: <string>)
  last(split(log.name, '.'))
end;

// Instances of this class are used as placeholders in the log hierarchy when
// a log is created before its parents are created.  i.e., if the first log
// created is named "x.y.z" then both x and x.y will be <placeholder-log>s.
// (If x.y is later created as a real log then the placeholder will be replaced.)
//
define open class <placeholder-log> (<abstract-log>)
end;

define public sealed generic level (log :: <log>) => (level :: <level>);
define public sealed generic targets (log :: <log>) => (targets :: <vector>);
define public sealed generic formatter (log :: <log>) => (formatter :: <formatter>);

define public open class <log> (<abstract-log>)
  public slot level :: <level> = $trace-level,
    init-keyword: level:;

  constant slot targets :: <stretchy-vector> = make(<stretchy-vector>),
    init-keyword: targets:;

  slot formatter :: <formatter> = $default-formatter,
    init-keyword: formatter:;

end class <log>;

define method make
    (class :: subclass(<log>),
     #rest args,
     #key formatter, targets :: false-or(<sequence>))
 => (log)
  // Formatter may be specified as a string for convenience.
  if (instance?(formatter, <string>))
    formatter := make(<formatter>, pattern: formatter);
  end;
  // Make sure targets is a <stretchy-vector>.  It's convenient for users
  // to be able to pass list(make(<target> ...)) though.
  let targets = as(<stretchy-vector>, targets | #[]);
  apply(next-method, class,
        targets: targets,
        formatter: formatter | $default-formatter,
        args)
end method make;

define method print-object
    (log :: <log>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format:to-stream(stream, "%s (%sadditive, level: %s, targets: %s)",
                     log.name,
                     if (log.additive?) "" else "non-" end,
                     log.level.level-name,
                     if (empty?(log.targets))
                       "None"
                     else
                       join(log.targets, ", ", key: curry(format-to-string, "%s"))
                     end);
  end;
end method print-object;

// There's only one add-target method, and we want the function to be
// public so I'm using this as an example of one of my other ideas, to
// allow "define generic" to accept a default implementation...
define public generic add-target
    (log :: <log>, target :: <target>) => ()
  add-new!(log.targets, target)
end;

define public generic remove-target
    (log :: <log>, target :: <target>) => ()
  remove!(log.targets, target);
end;

define public generic remove-all-targets
    (log :: <log>) => ()
  for (target in log.targets)
    remove-target(log, target)
  end;
end;

define open class <error> (dylan:<error>, <simple-condition>)
end;

define function %error
    (control-string, #rest args)
  signal(make(log:<error>,
              format-string: control-string,
              format-arguments: args))
end;

define public function get-root-log
    () => (log :: <log>)
  $root
end;

define public function get-log
    (name :: <string>) => (log :: false-or(<abstract-log>))
  %get-log($root, as(<list>, split(name, '.')), name)
end;

define method %get-log
    (log :: <abstract-log>, path :: <list>, original-name :: <string>)
  if (empty?(path))
    log
  else
    let child = element(log.children, first(path), default: #f);
    child & %get-log(child, path.tail, original-name)
  end
end method %get-log;

define method %get-log
    (log :: <placeholder-log>, path :: <list>, original-name :: <string>)
  ~empty?(path) & next-method()
end method %get-log;

define method %get-log
    (log == #f, path :: <list>, original-name :: <string>)
  %error("Log not found: %s", original-name);
end method %get-log;


define function add-log
    (parent :: <abstract-log>, new :: <abstract-log>, path :: <list>,
     original-name :: <string>)
  let name :: <string> = first(path);
  let child = element(parent.children, name, default: #f);
  if (path.size == 1)
    if (child)
      if (instance?(child, <placeholder-log>))
        // Copy the placeholder's children into the new log that
        // is replacing it.
        for (grandchild in child.children)
          new.children[local-name(grandchild)] := grandchild;
          grandchild.parent := new;
        end;
      else
        %error("Invalid log name, %s.  A child log named %s "
                 "already exists.", original-name, name);
      end;
    end;
    parent.children[name] := new;
    new.parent := parent;
  else
    if (~child)
      child := make(<placeholder-log>, name: name, parent: parent);
      parent.children[name] := child;
    end;
    log:add(child, new, path.tail, original-name);
  end;
end function add-log;




///////////////////////////////////////////////////////////
//// Log levels
////

define public generic level-name (level :: <level>) => (name :: <string>);

// Root of the log level hierarchy.  Logging uses a simple class
// hierarchy to determine what messages should be logged.
//
define public open abstract primary class <level> (<object>)
  constant slot level-name :: <byte-string>,
    init-keyword: name:;
end;

define public open class <trace-level> (<level>)
  inherited slot level-name = "trace";
end;

define public open class <debug-level> (<trace-level>)
  inherited slot level-name = "debug";
end;

define public open class <info-level> (<debug-level>)
  inherited slot level-name = "info";
end;

define public open class <warn-level> (<info-level>)
  inherited slot level-name = "WARN";
end;

define public open class <error-level> (<warn-level>)
  inherited slot level-name = "ERROR";
end;

define public constant $trace-level = make(<trace-level>);
define public constant $debug-level = make(<debug-level>);
define public constant $info-level = make(<info-level>);
define public constant $warn-level = make(<warn-level>);
define public constant $error-level = make(<error-level>);

define method level-applicable?
    (given-level :: <level>, level :: <level>)
 => (applicable? :: <boolean>)
  instance?(given-level, level.object-class)
end;




///////////////////////////////////////////////////////////
//// Logging messages
////

// This is generally called via info, error, etc, which simply curry
// the first argument.
//
define public generic message
    (given-level :: <level>, log :: <log>, object :: <object>, #rest args)
 => ()
  if (log.enabled?  & level-applicable?(given-level, log.level))
    for (target :: <target> in log.targets)
      to-target(target, given-level, log.formatter, object, args);
    end;
  end;
  if (log.additive?)
    apply(message, given-level, log.parent, object, args);
  end;
end method message;

define method message
    (given-level :: <level>, log :: <placeholder-log>, object :: <object>,
     #rest args)
  if (log.additive?)
    apply(message, given-level, log.parent, object, args)
  end;
end;

// I'm not sure trace is a useful distinction from debug.
// I copied it from log4j terminology.  I dropped fatal.
// TODO(cgay): these days I would probably drop trace and keep
// fatal.  It's a nice way to exit the program with a backtrace.

define public constant trace = curry(message, $trace-level);

define public constant debug = curry(message, $debug-level);

define public inline function debug-if
    (test, log :: <abstract-log>, object, #rest args)
  if (test)
    apply(debug, log, object, args);
  end;
end;

define public constant info = curry(message, $info-level);

define public constant warning = curry(message, $warn-level);

define public constant error = curry(message, $error-level);


///////////////////////////////////////////////////////////
//// Targets
////

// Abstract target for logging.  Subclasses represent different
// backend targets such as streams, files, databases, etc.
//
define public open abstract class <target> (<closable-object>)
end;


// When this is called, the decision has already been made that this object
// must be logged for the given log level, so methods should unconditionally
// write the object to the backing store.
//
define public open generic to-target
    (target :: <target>, level :: <level>, formatter :: <formatter>,
     object :: <object>, args :: <sequence>)
 => ();

// Override this if you want to use a normal formatter string but
// want to write objects to the log stream instead of strings.
//
define public open generic write-message
    (target :: <target>, object :: <object>, args :: <sequence>)
 => ();


// Note that there is no default method on "object :: <object>".

define method close
    (target :: <target>, #key)
 => ()
  // do nothing
end;

// A log target that simply discards its output.
define public sealed class <null-target> (<target>)
end;

define sealed method to-target
    (target :: <null-target>, level :: <level>,
     formatter :: <formatter>, format-string :: <string>,
     args :: <sequence>)
 => ()
  // do nothing
end;

define public constant $null-target :: <null-target>
  = make(<null-target>);

// A log target that outputs directly to a stream.
// e.g., make(<stream-target>, stream: *standard-output*)
//
define public open class <stream-target> (<target>)
  constant slot target-stream :: <stream>,
    required-init-keyword: #"stream";
end;

define method common-extensions:print-object
    (target :: <stream-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    write(stream, "stream target");
  end;
end method print-object;

define public constant $stdout-target
  = make(<stream-target>, stream: *standard-output*);

define public constant $stderr-target
  = make(<stream-target>, stream: *standard-error*);

define method to-target
    (target :: <stream-target>, level :: <level>, formatter :: <formatter>,
     format-string :: <string>, args :: <sequence>)
 => ()
  let stream :: <stream> = target.target-stream;
  streams:with-lock (stream)
    pattern-to-stream(formatter, stream, level, target, format-string, args);
    streams:write(stream, "\n");
    streams:flush(stream);
  end;
end method to-target;

define method write-message
    (target :: <stream-target>, format-string :: <string>, args :: <sequence>)
 => ()
  apply(format:to-stream, target.target-stream, format-string, args);
end method write-message;


// A log target that is backed by a single, monolithic file.
// (Why is this not a subclass of <stream-target>?)
//
define public class <file-target> (<target>)
  constant slot target-pathname :: fs:<pathname>,
    required-init-keyword: pathname:;
  slot target-stream :: false-or(fs:<file-stream>) = #f;
end;

define method initialize
    (target :: <file-target>, #key)
  next-method();
  open-target-stream(target);
end;

// The use of the common-extensions: module prefix here makes it clear to
// the reader that this extends a generic defined in a different module.
// The prefix isn't strictly necessary.
define method common-extensions:print-object
    (target :: <file-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format:to-stream(stream, "file %s", as(<string>, target.target-pathname));
  end;
end method print-object;

define public open generic open-target-stream
    (target :: <file-target>) => (stream :: <stream>);

define method open-target-stream
    (target :: <file-target>)
 => (stream :: fs:<file-stream>)
  fs:ensure-directories-exist(target.target-pathname);
  target.target-stream := make(fs:<file-stream>,
                               locator: target.target-pathname,
                               element-type: <character>,
                               direction: #"output",
                               if-exists: #"append",
                               if-does-not-exist: #"create")
end;

define method to-target
    (target :: <file-target>, level :: <level>,
     formatter :: <formatter>, format-string :: <string>,
     format-args :: <sequence>)
 => ()
  let stream :: streams:<stream> = target.target-stream;
  streams:with-lock (stream)
    pattern-to-stream(formatter, stream, level, target, format-string, format-args);
    streams:write(stream, "\n");
    streams:flush(stream);
  end;
end method to-target;

define method write-message
    (target :: <file-target>, format-string :: <string>, args :: <sequence>)
 => ()
  apply(format:to-stream, target.target-stream, format-string, args);
end;

define method close
    (target :: <file-target>, #key abort?)
 => ()
  if (target.target-stream)
    close(target.target-stream, abort?: abort?);
  end;
end;

// A log target that is backed by a file and ensures that the file
// only grows to a certain size, after which it is renamed to
// filename.<date-when-file-was-opened>.
//
// I investigated making this a subclass of <wrapper-stream> but it
// didn't work well due to the need to create the inner-stream
// first and pass it as an init arg.  That doesn't work too well
// given that I want to roll the log if the file exists when I
// first attempt to open it.  It leads to various special cases.
//
// Attempt to re-open the file if logging to it gets (the equivalent
// of) bad file descriptor?
//
define public class <rolling-file-target> (<file-target>)

  constant slot max-file-size :: <integer> = 100 * 1024 * 1024,
    init-keyword: max-size:;

  // TODO: not yet implemented
  // If this is #f then all versions are kept.
  //constant slot keep-versions :: false-or(<integer>) = #f,
  //  init-keyword: #"keep-versions";

  // TODO: not yet implemented
  //constant slot compress-on-close? :: <boolean> = #t,
  //  init-keyword: #"compress?";

  // Date when the underlying file was created.  When it gets closed
  // it will be renamed with this date in the name.
  slot file-creation-date <date> = date:now();

end class <rolling-file-target>;

define constant $roller-lock :: <lock> = make(<lock>);


define method initialize
    (target :: <rolling-file-target>, #key roll :: <boolean> = #t)
  if (roll
        & fs:file-exists?(target.target-pathname)
        & fs:file-property(target.target-pathname, #"size") > 0)
    roll-file(target);
  end;
  next-method();
end method initialize;

define method common-extensions:print-object
    (target :: <rolling-file-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format:to-stream(stream, "rolling file %s", as(<string>, target.target-pathname));
  end;
end method print-object;

define method to-target
    (target :: <rolling-file-target>, level :: <level>,
     formatter :: <formatter>, format-string :: <string>,
     format-args :: <sequence>)
 => ()
  next-method();
  // todo -- calling streams:size may be very slow?  Maybe to-target should
  // return the number of bytes written, but that could be inefficient (e.g.,
  // it might have to format to string and then write that to the underlying
  // stream instead of formatting directly to the stream).
  if (streams:size(target.target-stream) >= target.max-file-size)
    roll-file(target);
  end;
end;

define method roll-file
    (target :: <rolling-file-target>)
  with-lock ($roller-lock)
    if (target.target-stream)  // may be #f first time
      close(target.target-stream);
    end;
    // todo -- make the archived log filename accept %{date:fmt} and
    //         %{version} escapes.  e.g., "foo.log.%{version}"
    // Also consider putting more info in the rolled filenames, such
    // as process id, hostname, etc.  Makes it easier to combine files
    // into a single location.
    let date = date:to-string("%Y%m%dT%H%M%S", target.file-creation-date);
    let oldloc = as(<file-locator>, target.target-pathname);
    let newloc = merge-locators(as(<file-locator>,
                                   concatenate(locator-name(oldloc), ".", date)),
                                oldloc);
    fs:rename-file(oldloc, newloc);
    target.file-creation-date := date:now();
    open-target-stream(target);
  end with-lock;
end method roll-file;


///////////////////////////////////////////////////////////
//// Formatting
////

define public open class <formatter> (<object>)
  constant slot formatter-pattern :: <string>,
    required-init-keyword: pattern:;
  slot parsed-pattern :: <sequence>;
end class <formatter>;

// Leave in for debugging for now.
ignore(formatter-pattern);

define method initialize
    (formatter :: <formatter>, #key pattern :: <string>)
  next-method();
  formatter.parsed-pattern := parse-formatter-pattern(pattern);
end;

// Should be called with the stream locked.
//
define public generic pattern-to-stream
    (formatter :: <formatter>, stream :: <stream>,
     level :: <level>, target :: <target>,
     object :: <object>, args :: <sequence>)
 => ()
  for (item in formatter.parsed-pattern)
    if (instance?(item, <string>))
      write(stream, item);
    else
      // This is a little hokey, but it was easier to allow some
      // formatter functions to just return a string and others
      // to write to the underlying stream, so if the function
      // returns #f it means "i already did my output".
      let result = item(level, target, object, args);
      if (result)
        write(stream, result);
      end;
    end;
  end;
end method pattern-to-stream;

// Parse a string of the form "%{r} blah %{m} ..." into a list of functions
// and/or strings.  The functions can be called with no arguments and return
// strings.  The concatenation of all the resulting strings is the log message.
// (The concatenation needn't ever be done if writing to a stream, but I do
// wonder which would be faster, concatenation or multiple stream writes.
// Might be worth benchmarking at some point.)
//
define method parse-formatter-pattern
    (pattern :: <string>)
 => (parsed :: <sequence>)
  let result :: <stretchy-vector> = make(<stretchy-vector>);
  block (exit)
    let dispatch-char :: <byte-character> = '%';
    let index :: <integer> = 0;
    let control-size :: <integer> = pattern.size;
    local method next-char () => (char :: <character>)
            if (index >= control-size)
              %error("Log format control string ended prematurely: %s",
                     pattern);
            else
              let char = pattern[index];
              index := index + 1;
              char
            end
          end method;
    local method peek-char () => (char :: false-or(<character>))
            if (index < control-size)
              pattern[index]
            end
          end;
    while (index < control-size)
      // Skip to dispatch char.
      for (i :: <integer> = index then (i + 1),
           until: ((i == control-size)
                   | (pattern[i] == dispatch-char)))
      finally
        if (i ~== index)
          add!(result, copy-sequence(pattern, start: index, end: i));
        end;
        if (i == control-size)
          exit();
        else
          index := i + 1;
        end;
      end for;
      let start :: <integer> = index;
      let align :: <symbol> = #"right";
      let width :: <integer> = 0;
      let char = next-char();
      if (char == '-')
        align := #"left";
        char := next-char();
      end;
      if (member?(char, "0123456789"))
        let (wid, idx) = string-to-integer(pattern, start: index - 1);
        width := wid;
        index := idx;
        char := next-char();
      end;
      local method pad (string :: <string>)
              let len :: <integer> = string.size;
              if (width <= len)
                string
              else
                let fill :: <string> = make(<string>, size: width - len, fill: ' ');
                if (align == #"left")
                  concatenate(string, fill)
                else
                  concatenate(fill, string)
                end
              end
            end method;
      local method parse-long-format-control ()
              let bpos = index;
              while (~member?(peek-char(), ":}")) next-char() end;
              let word = copy-sequence(pattern, start: bpos, end: index);
              let arg = #f;
              if (pattern[index] == ':')
                next-char();
                let start = index;
                while(peek-char() ~= '}') next-char() end;
                arg := copy-sequence(pattern, start: start, end: index);
              end;
              next-char();   // eat '}'
              select (word by \=)
                "date" =>
                  method (#rest args)
                    pad(if (arg)
                          date:to-string(arg, date:now())
                        else
                          date:iso8601(date:now())
                        end)
                  end;
                "level" =>
                  method (level, target, object, args)
                    pad(level-name(level))
                  end;
                "message" =>
                  method (level, target, object, args)
                    write-message(target, object, args);
                    #f
                  end;
                "pid" =>
                  method (#rest args)
                    pad(integer-to-string(current-process-id()));
                  end;
                "millis" =>
                  method (#rest args)
                    pad(number-to-string(elapsed-milliseconds()));
                  end;
                "thread" =>
                  method (#rest args)
                    pad(thread-name(current-thread()));
                  end;
                otherwise =>
                  // Unknown control string.  Just output the text we've seen...
                  copy-sequence(pattern, start: start, end: index);
              end select;
            end method;
      add!(result,
           select (char)
             '{' => parse-long-format-control();
             'd' =>
               method (#rest args)
                 pad(date:iso8601(date:now()));
               end;
             'l', 'L' =>
               method (level, target, object, args)
                 pad(level-name(level))
               end;
             'm' =>
               method (level, target, object, args)
                 write-message(target, object, args);
                 #f
               end;
             'p' =>
               method (#rest args)
                 pad(integer-to-string(current-process-id()));
               end;
             'r' =>
               method (#rest args)
                 pad(number-to-string(elapsed-milliseconds()));
               end;
             't' =>
               method (#rest args)
                 pad(thread-name(current-thread()));
               end;
             '%' => pad("%");
             otherwise =>
               // Unknown control char.  Just output the text we've seen...
               copy-sequence(pattern, start: start, end: index);
           end);
    end while;
  end block;
  result
end method parse-formatter-pattern;

define public constant $default-formatter :: <formatter>
  = make(<formatter>, pattern: "%{date:%Y-%m-%dT%H:%M:%S.%F%z} %-5L [%t] %m");

define constant $application-start-date :: <date> = date:now();

define function elapsed-milliseconds
    () => (millis :: <double-integer>)
  let duration :: <duration> = date:now() - $application-start-date;
  let (days, hours, minutes, seconds, microseconds) = date:decode-duration(duration);
  plus(div(microseconds, 1000.0),
       plus(mul(seconds, 1000),
            plus(mul(minutes, 60000),
                 plus(mul(hours, 3600000), mul(days, 86400000)))))
end function elapsed-milliseconds;


/////////////////////////////////////////////////////
//// For use by the test suite
////

define public function %reset
    ()
  // maybe should close existing log targets?
  $root := make(<log>, name: "root", additive?: #f, enabled?: #f);
end;

/////////////////////////////////////////////////////
//// Initialize
////

begin
  %reset();
end;


