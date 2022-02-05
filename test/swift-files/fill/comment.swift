// swift-mode:test:case-begin (Slash style doc comment)

/// Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
/// tempor incididunt ut labore et dolore magna aliqua. Ut
///
/// - parameter foo: enim ad minim veniam, quis nostrud exercitation ullamco
/// laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
/// reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
/// pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui
/// officia deserunt mollit anim id est laborum.
/// - parameter bar: enim ad minim veniam, quis nostrud exercitation ullamco
/// - returns: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
/// eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
/// veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
/// commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
/// esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
/// cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
/// est laborum.
///
/// - warning: reprehenderit in voluptate velit esse

// swift-mode:test:case-input-begin

/// Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et
/// dolore magna aliqua. Ut
///
/// - parameter foo: enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor
/// in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
/// - parameter bar: enim ad minim veniam, quis nostrud exercitation ullamco
/// - returns: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
///
/// - warning: reprehenderit in voluptate velit esse

// swift-mode:test:case-end (Slash style doc comment)

// swift-mode:test:eval (setq swift-mode:comment-fill-column 96)
// swift-mode:test:case-begin (Slash line comment)

// Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut
// labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco

// swift-mode:test:case-input-begin

// Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna
// aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco

// swift-mode:test:case-end

// swift-mode:test:eval (setq swift-mode:comment-fill-column 80)
// swift-mode:test:case-begin (Star style doc comment)

/**
 Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
 incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
 nostrud exercitation ullamco

 - parameter foo: laboris nisi ut
 - parameter bar: aliquip ex ea commodo consequat. Duis aute irure dolor in
 reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
 pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui
 officia deserunt mollit anim id est laborum.
 - parameter baz: enim ad minim veniam, quis nostrud exercitation ullamco

 - returns: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
 eiusmod tempor incididunt ut labore et dolore magna aliqua.

 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
 aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in
 voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
 occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim
 id est laborum.

 - important: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
 eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
 veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
 consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
 cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
 proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
 */

// swift-mode:test:case-input-begin

/**
 Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
 enim ad minim veniam, quis nostrud exercitation ullamco

 - parameter foo: laboris nisi ut
 - parameter bar: aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit
 in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
 occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim
 id est laborum.
 - parameter baz: enim ad minim veniam, quis nostrud exercitation ullamco

 - returns: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore
 et dolore magna aliqua.

 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

 - important: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
 */

// swift-mode:test:case-end (Star style doc comment)

// swift-mode:test:case-begin (Star comment, single line)

/**
 Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
 incididunt ut
 */

// swift-mode:test:case-input-begin

/** Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut */

// swift-mode:test:case-end (Star comment, single line)
