use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
};

pub struct CodeGen<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
}

impl<'ctx, 'a> CodeGen<'ctx, 'a> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
        }
    }

    pub fn codegen(&self) {
        let i32_type = self.context.i32_type();
        let void_type = self.context.void_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_value = self.module.add_function("main", fn_type, None);
        let getint_fn_type = i32_type.fn_type(&[], false);
        let putint_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let putch_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let getint_fn_value =
            self.module
                .add_function("getint", getint_fn_type, Some(Linkage::External));
        let putint_fn_value =
            self.module
                .add_function("putint", putint_fn_type, Some(Linkage::External));
        let putch_fn_value =
            self.module
                .add_function("putch", putch_fn_type, Some(Linkage::External));
        let basic_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(basic_block);
        let const_int = i32_type.const_int(12123, false);
        self.builder.build_call(putint_fn_value, &[const_int.into()], "");
        let enter_int = i32_type.const_int(10, false);
        self.builder.build_call(putch_fn_value, &[enter_int.into()], "");

        self.builder.build_return(Some(&const_int));
    }
}
