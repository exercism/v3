struct Position {
  var x = 0
  var y = 0

  mutating func moveTo(newX: Int, newY: Int) {
    x = newX
    y = newY
  }
}

struct Size {
  var width = 80
  var height = 60

  mutating func resize(newWidth: Int, newHeight: Int) {
    width = newWidth
    height = newHeight
  }
}

class Window {
  var title = "New Window"
  let screenSize = Size(width: 800, height: 600)
  var size = Size()
  var position = Position()
  var contents: String?

  func move(to newPosition: Position) {
    let minX = min(max(0, newPosition.x), screenSize.width - size.width)
    let minY = min(max(0, newPosition.y), screenSize.height - size.height)
    position.moveTo(newX: minX, newY: minY)
  }
  func resize(to newSize: Size) {
    let minW = min(max(1, newSize.width), screenSize.width - position.x)
    let minH = min(max(1, newSize.height), screenSize.height - position.y)
    size.resize(newWidth: minW, newHeight: minH)
  }
  func update(title: String) {
    self.title = title
  }
  func update(text: String?) {
    contents = text
  }
  func display() -> String {
    "\(title)\nPosition: (\(position.x), \(position.y)), Size: (\(size.width) x \(size.height))\n\(contents ?? "[This window intentionally left blank]")\n"
  }
}

let mainWindow: Window = {
  var window = Window()
  window.title = "Main Window"
  window.contents = "This is the main window"
  window.move(to: Position(x: 100, y: 100))
  window.resize(to: Size(width: 400, height: 300))
  return window
}()
