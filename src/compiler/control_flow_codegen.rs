use super::{
    expr_codegen::codegen_rhs_expr,
    helpers::{BlockType, Compiler},
    main_codegen::{codegen_block, codegen_stmt},
};
use crate::ast::{DoWhile, For, If, ValueVarType, VarType, While};
use std::collections::VecDeque;

pub fn codegen_if(
    compiler: &mut Compiler,
    if_: If,
    mut block_type: BlockType,
) -> Result<(), String> {
    block_type.insert(BlockType::IF);

    let parent_block = compiler.builder.get_insert_block().unwrap();
    let parent_func = parent_block.get_parent().unwrap();

    let then_block = compiler.context.append_basic_block(parent_func, "then");
    let else_block = compiler.context.append_basic_block(parent_func, "else");
    let merge_block = compiler.context.append_basic_block(parent_func, "ifcont");

    let (if_expr, if_expr_type) = codegen_rhs_expr(
        compiler,
        if_.if_expr,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
        block_type,
    )
    .map_err(|_| "Invalid expression for if condition")?;
    if if_expr_type.array_dimensions.len() > 0
        || if_expr_type.pointer_nesting_level > 0
        || !matches!(if_expr_type.vtype, VarType::Boolean)
    {
        return Err("Conditional expression must be of boolean type".to_string());
    }

    // Then
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, if_.if_block, block_type)?;
    compiler.builder.build_unconditional_branch(merge_block);

    // Else ifs
    let else_ifs = if_.else_if;
    let mut else_if_blocks = vec![];

    let mut build_after_then = then_block;
    for _ in 0..else_ifs.len() {
        let else_if_cond_bb = compiler
            .context
            .insert_basic_block_after(build_after_then, "else_if_cond");
        build_after_then = else_if_cond_bb;
        let else_if_block_bb = compiler
            .context
            .insert_basic_block_after(build_after_then, "else_if_block");
        build_after_then = else_if_block_bb;

        else_if_blocks.push((else_if_cond_bb, else_if_block_bb));
    }

    let mut next_cond_bb = else_block;
    for (else_if, (else_if_cond_bb, else_if_block_bb)) in
        else_ifs.into_iter().zip(else_if_blocks).rev()
    {
        // Condition
        compiler.builder.position_at_end(else_if_cond_bb);
        let (comp, comp_type) =
            codegen_rhs_expr(compiler, else_if.else_expr, None, block_type).unwrap();
        if comp_type.array_dimensions.len() > 0
            || comp_type.pointer_nesting_level > 0
            || !matches!(comp_type.vtype, VarType::Boolean)
        {
            return Err("Conditional expression must be of boolean type".to_string());
        }

        compiler.builder.build_conditional_branch(
            comp.into_int_value(),
            else_if_block_bb,
            next_cond_bb,
        );
        next_cond_bb = else_if_cond_bb;

        // Block
        compiler.builder.position_at_end(else_if_block_bb);
        codegen_block(compiler, else_if.else_block, block_type)?;
        compiler.builder.build_unconditional_branch(merge_block);
    }

    // Else
    compiler.builder.position_at_end(else_block);
    if let Some(else_b) = if_.else_b {
        codegen_block(compiler, else_b, block_type)?;
    }
    compiler.builder.build_unconditional_branch(merge_block);

    compiler.builder.position_at_end(parent_block);
    // If
    compiler
        .builder
        .build_conditional_branch(if_expr.into_int_value(), then_block, next_cond_bb);

    compiler.builder.position_at_end(merge_block);

    Ok(())
}

pub fn codegen_while(
    compiler: &mut Compiler,
    while_: While,
    mut block_type: BlockType,
) -> Result<(), String> {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "while_cond");
    let then_block = compiler
        .context
        .append_basic_block(parent_block, "while_then");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "while_cont");

    // While
    compiler.builder.build_unconditional_branch(comp_block);
    compiler.builder.position_at_end(comp_block);
    let while_expr = codegen_rhs_expr(
        compiler,
        while_.while_cond,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
        block_type,
    )
    .expect("Invalid expression for while condtition")
    .0
    .into_int_value();
    compiler
        .builder
        .build_conditional_branch(while_expr, then_block, merge_block);

    // Then
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, while_.block, block_type)?;
    compiler.builder.build_unconditional_branch(comp_block);

    compiler.builder.position_at_end(merge_block);

    Ok(())
}

pub fn codegen_do_while(
    compiler: &mut Compiler,
    do_while: DoWhile,
    mut block_type: BlockType,
) -> Result<(), String> {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let then_block = compiler
        .context
        .append_basic_block(parent_block, "while_then");
    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "while_cond");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "while_cont");

    // Do
    compiler.builder.build_unconditional_branch(then_block);
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, do_while.do_block, block_type)?;
    compiler.builder.build_unconditional_branch(comp_block);

    // While
    compiler.builder.position_at_end(comp_block);
    let while_expr = codegen_rhs_expr(
        compiler,
        do_while.while_cond,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
        block_type,
    )
    .expect("Invalid expression for do while condtition")
    .0
    .into_int_value();
    compiler
        .builder
        .build_conditional_branch(while_expr, then_block, merge_block);

    // Cont
    compiler.builder.position_at_end(merge_block);

    Ok(())
}

pub fn codegen_for(
    compiler: &mut Compiler,
    for_: For,
    mut block_type: BlockType,
) -> Result<(), String> {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let decl_block = compiler
        .context
        .append_basic_block(parent_block, "for_decl");
    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "for_cond");
    let postfix_block = compiler
        .context
        .append_basic_block(parent_block, "for_postfix");
    let then_block = compiler
        .context
        .append_basic_block(parent_block, "for_then");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "for_cont");

    // Decl stmt
    compiler.builder.build_unconditional_branch(decl_block);
    compiler.builder.position_at_end(decl_block);
    if let Some(stmt) = for_.decl_stmt {
        codegen_stmt(compiler, *stmt, block_type)?;
    }
    compiler.builder.build_unconditional_branch(comp_block);

    // Postfix stmt
    compiler.builder.position_at_end(postfix_block);
    if let Some(stmt) = for_.postfix_stmt {
        codegen_stmt(compiler, *stmt, block_type)?;
    }
    compiler.builder.build_unconditional_branch(comp_block);

    // Block
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, for_.block, block_type)?;
    compiler.builder.build_unconditional_branch(postfix_block);

    // Cond
    compiler.builder.position_at_end(comp_block);
    let cond_expr = if let Some(cmp_expr) = for_.cmp_expr {
        codegen_rhs_expr(
            compiler,
            cmp_expr,
            Some(&ValueVarType {
                vtype: VarType::Boolean,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            }),
            block_type,
        )
        .expect("Invalid condtition expression in for loop")
        .0
        .into_int_value()
    } else {
        // Default value is true
        compiler.context.bool_type().const_int(1, false)
    };

    // Execution
    compiler
        .builder
        .build_conditional_branch(cond_expr, then_block, merge_block);

    // Cont
    compiler.builder.position_at_end(merge_block);

    Ok(())
}
