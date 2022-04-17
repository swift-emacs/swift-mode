// swift-mode:test:paragraph
public class Foo {
    // Code comment before and after documentation comment:

    // swift-mode:test:paragraph swift-mode:test:start-block
    //
    // swift-mode:test:paragraph swift-mode:test:end-block
    /// swift-mode:test:paragraph swift-mode:test:start-block
    ///
    /// swift-mode:test:paragraph swift-mode:test:end-block
    // swift-mode:test:paragraph swift-mode:test:start-block
    //
    // swift-mode:test:paragraph swift-mode:test:end-block

    // Comment box:

    ///////////////////////
    // swift-mode:test:paragraph swift-mode:test:start-block
    //
    // swift-mode:test:paragraph swift-mode:test:end-block
    ///////////////////////

    ///////////////////////
    ///////////////////////
    //
    // swift-mode:test:paragraph swift-mode:test:start-block
    //
    // swift-mode:test:paragraph swift-mode:test:end-block
    //
    ///////////////////////
    ///////////////////////

    /**********************
     swift-mode:test:paragraph swift-mode:test:start-block

     swift-mode:test:paragraph swift-mode:test:end-block
     *********************/


    // Code comment before and after comment box:

    // swift-mode:test:paragraph
    //
    // swift-mode:test:paragraph
    ////////////////////////////
    /// swift-mode:test:paragraph swift-mode:test:start-block
    ///
    /// swift-mode:test:paragraph swift-mode:test:end-block
    ////////////////////////////
    // swift-mode:test:paragraph
    //
    // swift-mode:test:paragraph

    /*
     swift-mode:test:paragraph

     swift-mode:test:paragraph
     */
    /**
     swift-mode:test:paragraph swift-mode:test:start-block

     swift-mode:test:paragraph swift-mode:test:end-block
     */
    /*
     swift-mode:test:paragraph

     swift-mode:test:paragraph
     */


    // Code and comments

    /// swift-mode:test:paragraph
    func foo() {}

    /// swift-mode:test:paragraph
    func foo() {}

    /**
     swift-mode:test:paragraph
     */
    func foo() {}

    /**
     swift-mode:test:paragraph
     */
    func foo() {}


    // CommonMark

    // swift-mode:test:paragraph swift-mode:test:start-block
    // swift-mode:test:heading
    // swift-mode:test:list-item
    // swift-mode:test:list-item
    // swift-mode:test:list-item
    // ```
    // swift-mode:test:paragraph
    // ```
    // swift-mode:test:paragraph
    // - - -
    // swift-mode:test:paragraph
    // =========================
    // swift-mode:test:paragraph
    // -------------------------
    // swift-mode:test:paragraph swift-mode:test:end-block

    /**
     swift-mode:test:paragraph swift-mode:test:start-block
     swift-mode:test:heading
     swift-mode:test:list-item
     swift-mode:test:list-item
     swift-mode:test:list-item
     ```
     swift-mode:test:paragraph
     ```
     swift-mode:test:paragraph
     - - -
     swift-mode:test:paragraph
     =========================
     swift-mode:test:paragraph
     -------------------------
     swift-mode:test:paragraph swift-mode:test:end-block
     */


    // Strings
    ###"""
      swift-mode:test:paragraph swift-mode:test:start-block

      swift-mode:test:paragraph swift-mode:test:end-block
      """###
}
// swift-mode:test:paragraph
