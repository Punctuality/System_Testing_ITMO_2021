package ru.ifmo.se.software.testing.lab3

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.junit.Assert._
import org.junit._
import org.openqa.selenium._
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.safari.SafariDriver
import org.openqa.selenium.support.ui.WebDriverWait

import java.time.Duration

class LoginCaseSpec {
  private implicit def unitToIO(action: => Unit): IO[Unit] = IO(action)

  private val profileUrl: String = "https://www.ivi.ru/profile"
  private val driverRef = Ref[IO].of[WebDriver](null).unsafeRunSync()
  private val pageCaseRef = Ref[IO].of[LogicCase[IO]](null).unsafeRunSync()
  private val creds = LogicCase.Credentials(System.getenv("login"), System.getenv("password"))

  @Before
  def setUp(): Unit = (for {
    _ <- unitToIO { System.setProperty("webdriver.chrome.driver", "src/test/resources/chromedriver")}
    driver <- driverRef.updateAndGet(_ => new ChromeDriver)
    _ <- {
      driver.manage.window.maximize()
      driver.manage.timeouts.implicitlyWait(Duration.ofSeconds(10))
      driver.get(profileUrl)
    }
    _ <- pageCaseRef.updateAndGet(_ => new LogicCase[IO](driver))
  } yield ()).unsafeRunSync()

  @After
  def tearDown(): Unit =
    driverRef.get.map(_.quit()).unsafeRunSync()

  @Test
  def login(): Unit = (
      for {
        page <- pageCaseRef.get
        _ <- page.loginProcedure(creds)
        _ <- page.pickProfileProcedure
      } yield assertEquals(creds.login.split('@').head, page.loginName.getText)
    ).unsafeRunSync()

  @Test
  def invalidLogin(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.loginProcedure(creds.copy(password = creds.password + "_wrong"))
    } yield assertEquals("Ошибка", page.loginFailureMessage.getText)
    ).unsafeRunSync()
}
