package CS331.errors;

/** Exception class thrown when a lexical error is encountered. */
public class LexicalError extends CompilerError
{
   public LexicalError(Type errorNumber, String message)
   {
      super(errorNumber, message);
   }

   // Factory methods to generate the lexical exception types.

   public static LexicalError BadComment(int n)
   {
      return new LexicalError(Type.BAD_COMMENT,
                              ">>> ERROR: Cannont include { inside a comment"  + " at line" + n);
   }

    public static LexicalError IllegalCharacter(char c, int n)
   {
      return new LexicalError(Type.ILLEGAL_CHARACTER,
                              ">>> ERROR: Illegal character: " + c + " at line" + n);
   }

   public static LexicalError UnterminatedComment(int n)
   {
      return new LexicalError(Type.UNTERMINATED_COMMENT,
                              ">>> ERROR: Unterminated comment "+ "at line" + n);
   }
}
