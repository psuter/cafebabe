object Test {
  def main(args : Array[String]) : Unit = {
    import cafebabe.AbstractByteCodes._
    import cafebabe.ByteCodes._
    
    val cf = new cafebabe.ClassFile("Test", None)
    
    cf.addField("Ljava/lang/String;", "name")
    
    cf.addDefaultConstructor
    
    {
      val ch = cf.addMethod("I", "sayHello", "I", "Z", "Ljava/lang/String;").codeHandler
      ch << Ldc(41)
      ch << DefaultNew("Oh") << Ldc(3.14) << InvokeVirtual("Oh", "getInt", "(D)I") << IADD
      ch << IRETURN
      ch.freeze
    }
    
    {
      val ch = cf.addMethod("I", "jumpOver").codeHandler
      ch << Ldc(42)
      val fresh: Int = ch.getFreshVar
      ch << Goto("after")
      ch << POP << Ldc(41)
      ch << Label("after")
      ch << IRETURN
      ch.freeze
    }
    
    {
      val ch = cf.addMethod("I", "fact", "I").codeHandler
      val label = ch.getFreshLabel("else")
      ch << ILoad(1) << Ldc(1) << If_ICmpGt(label) << Ldc(1) <<
               IRETURN << Label(label) << ILoad(1) << ALoad(0) <<
               ILoad(1) << Ldc(1) << ISUB << InvokeVirtual("Test", "fact", "(I)I") <<
               IMUL << IRETURN
      
      ch.freeze
    }
    
    cf.writeToFile("./classfiles/Test.class")
    
    val classFile = new cafebabe.ClassFile("HWGenerated", None)
    classFile.addDefaultConstructor
    val codeHandler = classFile.addMainMethod.codeHandler
    
    codeHandler << 
      GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") << 
      Ldc("Hello world!") <<
      InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
      RETURN
    
    codeHandler.freeze
    classFile.writeToFile("./classfiles/HWGenerated.class")
    
    System.out.println("written files.")
  }
}
