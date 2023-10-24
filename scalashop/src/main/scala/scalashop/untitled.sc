val width = 1920
val height = 1080
val numTasks = 32
val taskSize = (width.toFloat / numTasks.toFloat).ceil.toInt

val tasks = (0 until width by taskSize).zip((0 to width by taskSize).tail)
