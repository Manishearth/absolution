absolution_hack::declare_absolution_hack! {
    /// This macro adds one to it's input value in a very round-about way.
    pub use demo_hack_impl::__add_one_impl as add_one;
}
