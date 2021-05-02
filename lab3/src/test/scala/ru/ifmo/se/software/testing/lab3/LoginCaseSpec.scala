package ru.ifmo.se.software.testing.lab3

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.junit.Assert._
import org.junit._
import org.openqa.selenium._
import org.openqa.selenium.chrome.ChromeDriver

import java.time.Duration
import scala.util.Try

class LoginCaseSpec {
  private implicit def unitToIO(action: => Unit): IO[Unit] = IO(action)

  private val profileUrl: String = "https://www.ivi.ru/profile"
  private val driverRef = Ref[IO].of[WebDriver](null).unsafeRunSync()
  private val pageCaseRef = Ref[IO].of[LoginCase[IO]](null).unsafeRunSync()
  private val creds = LoginCase.Credentials(System.getenv("login"), System.getenv("password"))

  @Before
  def setUp(): Unit = (for {
    _ <- unitToIO { System.setProperty("webdriver.chrome.driver", "src/test/resources/chromedriver")}
    driver <- driverRef.updateAndGet(_ => new ChromeDriver)
    _ <- {
      driver.manage.window.maximize()
      driver.manage.timeouts.implicitlyWait(Duration.ofSeconds(10))
      driver.get(profileUrl)
    }
    _ <- pageCaseRef.updateAndGet(_ => new LoginCase[IO](driver))
  } yield ()).unsafeRunSync()

  @After
  def tearDown(): Unit =
    driverRef.get.map(_.quit()).unsafeRunSync()

  @Test
  def login(): Unit = (
      for {
        page <- pageCaseRef.get
        _ <- page.loginProcedure(creds)
        _ <- page.pickProfileProcedure()
      } yield assertEquals("Profile name should be same as in creds",creds.login.split('@').head, page.loginName.getText)
    ).unsafeRunSync()

  @Test
  def invalidLogin(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.loginProcedure(creds.copy(password = creds.password + "_wrong"))
      _ <- Thread.sleep(200)
    } yield assertEquals("Should provide error message","Ошибка", page.loginFailureMessage.getText)
    ).unsafeRunSync()

  @Test
  def signOut(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.loginProcedure(creds)
      _ <- page.pickProfileProcedure()
      _ <- page.signOutProcedure()
      _ <- Thread.sleep(200)
    } yield assertEquals("There should be no profile name", None, Try(page.loginName).toOption)
  ).unsafeRunSync()
}
