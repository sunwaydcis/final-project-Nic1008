package AssignmentSnake

import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.control.Label
import scalafx.scene.image.{Image, ImageView}

import scala.concurrent.Future
import scala.util.Random

object SnakePro extends JFXApp3 {

  val initialSnake: List[(Double, Double)] = List(
    (200, 200),
    (225, 200),
    (250, 200)
  )

  val score: IntegerProperty = IntegerProperty(0)

  import scala.concurrent.ExecutionContext.Implicits.global

  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1000 / 25 * 2)
    }.flatMap(_ => Future(gameLoop(update)))

  case class State(snake: List[(Double, Double)], food: (Double, Double)) {
    def newState(dir: Int): State = {
      val (x, y) = snake.head
      val (newx, newy) = dir match {
        case 1 => (x, y - 25)
        case 2 => (x, y + 25)
        case 3 => (x - 25, y)
        case 4 => (x + 25, y)
        case _ => (x, y)
      }

      val newSnake: List[(Double, Double)] =
        if (newx < 0 || newx >= 600 || newy < 0 || newy >= 600 || snake.tail.contains((newx, newy)))
          initialSnake
        else if (food == (newx, newy)) {
          score.value += 10
          println(s"Score updated: ${score.value}")
          food :: snake
        } else
          (newx, newy) :: snake.init

      val newFood =
        if (food == (newx, newy))
          randomFood()
        else
          food

      State(newSnake, newFood)
    }

    def rectangles: List[Rectangle] = {
      val head = rect(snake.head._1, snake.head._2, Blue) // Head in Blue
      val body = snake.tail.map { case (x, y) => rect(x, y, Green) }
      head :: rect(food._1, food._2, Red) :: body
    }
  }

  def randomFood(): (Double, Double) =
    (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  def rect(xr: Double, yr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = color
  }

  override def start(): Unit = {
    val mainImage = new Image(getClass.getResource("/snake-gamebackground.jpg").toExternalForm) // Replace with your background image path

    // Instructions Scene
    def showInstructionsScene(): Unit = {
      val imageView = new ImageView(mainImage) {
        fitWidth = 600
        fitHeight = 600
        preserveRatio = true
      }

      val instructionsScene = new Scene(400, 400) {
        fill = LightGray
        content = List(
          imageView,
          new Label("Game Instructions") {
            layoutX = 150
            layoutY = 40
            style = "-fx-font-size: 30px; -fx-text-fill: black;"
          },
          new Label("W = Move Up") {
            layoutX = 160
            layoutY = 120
            style = "-fx-font-size: 18px; -fx-text-fill: black;"
          },
          new Label("S = Move Down") {
            layoutX = 160
            layoutY = 150
            style = "-fx-font-size: 18px; -fx-text-fill: black;"
          },
          new Label("A = Move Left") {
            layoutX = 160
            layoutY = 180
            style = "-fx-font-size: 18px; -fx-text-fill: black;"
          },
          new Label("D = Move Right") {
            layoutX = 160
            layoutY = 210
            style = "-fx-font-size: 18px; -fx-text-fill: black;"
          },
          new Label("Avoid hitting walls or your own snake!") {
            layoutX = 120
            layoutY = 250
            style = "-fx-font-size: 18px; -fx-text-fill: black;"
          },
          new Label("Press Enter to Start the Game") {
            layoutX = 130
            layoutY = 300
            style = "-fx-font-size: 16px; -fx-text-fill: black;"
          }
        )

        onKeyPressed = key => key.getCode.getName match {
          case "Enter" => startGameScene()
          case "Escape" => Platform.exit()
          case _ =>
        }
      }

      stage.scene = instructionsScene
    }

    // Method to start the game scene
    def startGameScene(): Unit = {
      stage.width = 620
      stage.height = 640

      val gameImage = new Image(getClass.getResource("/Snake-Scene.jpg").toExternalForm) // Replace with your game scene image path
      val gameImageView = new ImageView(gameImage) {
        fitWidth = 700
        fitHeight = 900
        preserveRatio = true
      }

      val state = ObjectProperty(State(initialSnake, randomFood()))
      val frame = IntegerProperty(0)
      val direction = IntegerProperty(4) // Right

      val scoreLabel = new Label {
        text = "Score: 0"
        layoutX = 10
        layoutY = 10
        style = "-fx-font-size: 16px; -fx-text-fill: white;" // White text for better visibility
        score.onChange { (_, _, newValue) =>
          text = s"Score: $newValue"
        }
      }

      frame.onChange {
        state.update(state.value.newState(direction.value))
      }

      val gameScene = new Scene {
        fill = Black // A fallback color if the image doesn't load
        val updatedContent = gameImageView :: state.value.rectangles
        val finalContent = updatedContent :+ scoreLabel

        content = finalContent

        onKeyPressed = key => key.getText match {
          case "w" if direction.value != 2 => direction.value = 1
          case "s" if direction.value != 1 => direction.value = 2
          case "a" if direction.value != 4 => direction.value = 3
          case "d" if direction.value != 3 => direction.value = 4
        }

        frame.onChange {
          Platform.runLater {
            val updatedContent = gameImageView :: state.value.rectangles
            val finalContent = updatedContent :+ scoreLabel
            content = finalContent
          }
        }
      }

      stage.scene = gameScene
      gameLoop(() => frame.update(frame.value + 1))
    }

    val imageView = new ImageView(mainImage) {
      fitWidth = 600
      fitHeight = 600
      preserveRatio = true
    }

    // Main Menu Scene
    val menuScene = new Scene(400, 400) {
      fill = LightGray
      content = List(
        imageView,
        new Label("Snake Game") {
          layoutX = 190
          layoutY = 100
          style = "-fx-font-size: 40px; -fx-text-fill: black;"
        },
        new Label("Press Enter to Start") {
          layoutX = 195
          layoutY = 180
          style = "-fx-font-size: 25px; -fx-text-fill: black;"
        },
        new Label("Press I for Instructions") {
          layoutX = 223
          layoutY = 280
          style = "-fx-font-size: 16px; -fx-text-fill: black;"
        }
      )

      onKeyPressed = key => key.getCode.getName match {
        case "Enter" => startGameScene()
        case "I" => showInstructionsScene()
        case "Escape" => Platform.exit()
        case _ =>
      }
    }

    stage = new JFXApp3.PrimaryStage {
      width = 605
      height = 415
      scene = menuScene
    }
  }
}








