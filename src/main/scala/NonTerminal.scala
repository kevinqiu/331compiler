package parser

abstract class NonTerminal(val value:Int)

case object Program extends NonTerminal(0)
case object Identifier_list extends NonTerminal(1)
case object Declarations extends NonTerminal(2)
case object Sub_declarations extends NonTerminal(3)
case object Compound_statement extends NonTerminal(4)
case object Identifier_list_tail extends NonTerminal(5)
case object Seclaration_list extends NonTerminal(6)
case object Type extends NonTerminal(7)
case object Declaration_list_tail extends NonTerminal(8)
case object Standard_type extends NonTerminal(9)
case object Array_type extends NonTerminal(10)
case object Subprogram_declaration extends NonTerminal(11)
case object Subprogram_head extends NonTerminal(12)
case object Arguments extends NonTerminal(13)
case object Parameter_list extends NonTerminal(14)
case object Parameter_list_tail extends NonTerminal(15)
case object Statement_list extends NonTerminal(16)
case object Statement extends NonTerminal(17)
case object Statement_list_tail extends NonTerminal(18)
case object Elementary_statement extends NonTerminal(19)
case object Expression extends NonTerminal(20)
case object Else_clause extends NonTerminal(21)
case object Es_tail extends NonTerminal(22)
case object Subscript extends NonTerminal(23)
case object Parameters extends NonTerminal(24)
case object Expression_list extends NonTerminal(25)
case object Expression_list_tail extends NonTerminal(26)
case object Simple_expression extends NonTerminal(27)
case object Expression_tail extends NonTerminal(28)
case object Term extends NonTerminal(29)
case object Simple_expression_tail extends NonTerminal(30)
case object Sign extends NonTerminal(31)
case object Factor extends NonTerminal(32)
case object Term_tail extends NonTerminal(33)
case object Factor_tail extends NonTerminal(34)
case object Actual_parameters extends NonTerminal(35)
case object Goal extends NonTerminal(36)
case object Constant extends NonTerminal(37)
