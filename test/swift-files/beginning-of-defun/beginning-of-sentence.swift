

/*{*/let x = "abc./*]*/  /*[*/def."/*}*/

// /*[*/aaa
// bbb/*]*/
//
// /*[*/ccc
// ddd/*]*/

//
// /*[*/eee
// fff/*]*/
//

/////
///// /*[*/eee
///// fff/*]*/
/////

/**
 /*[*/aaa./*]*/  /*[*/bbb
 ccc/*]*/

 /*[*/eee/*]*/
 */

/*{*/let x = 1/*}*/ // /*[*/aaa
// aaa
// bbb/*]*/

/*{*/func foo() {
    /// /*[*/aaa./*]*/  /*[*/bbb./*]*/
    ///
    /// /*[*/ccc./*]*/
}/*}*/

/*{*/let x = """
  /*[*/aaa./*]*/
  /*[*/bbb./*]*/
  /*[*/ccc./*]*/
  """ + """
       /*[*/ddd./*]*/
       /*[*/eee./*]*/
       /*[*/fff./*]*/
       """/*}*/


/*{*/let x = """
  /*[*/aaa./*]*/
  /*[*/bbb \(
    foo("aaa./*]*/  /*[*/bbb") + { /*[*/() in/*]*/
        /*[*/foo()/*]*/
        /*[*/return bar()/*]*/
    }() + bar("aaa./*]*/  /*[*/bbb")
  ) ccc./*]*/
  """/*}*/


