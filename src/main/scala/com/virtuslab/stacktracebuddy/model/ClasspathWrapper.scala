package com.virtuslab.stacktracebuddy.model

import java.io.File

case class ClasspathWrapper(file: File, jarName: Option[String])
case class TastyWrapper(file: File, jarName: Option[String])
