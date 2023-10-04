# Contribution to

This outlines how to propose a change to the aok package. For more detailed info
about contributing to {oak}, and other [pharmaverse
packages](https://pharmaverse.org/), please see the [development process
guide](https://pharmaverse.github.io/admiraldev/main/articles/development_process.html)
as well as other Developer Guides in the Articles section of the [{admiral}
website](https://pharmaverse.github.io/admiral/cran-release/index.html)

# Basics

* For each new contribution, the user creates an issue on the issue tab on [GitHub](https://github.com/pharmaverse/oak/issues) to put it in our backlog.
  The issues can range from bug identification and/or fixes, enhancements to
  functions, documentation, tests or new features.
* We advise you to contact us when an
  [issue](https://github.com/pharmaverse/oak/issues) is created via
  [Slack](https://oakgarden.slack.com) (If you don't have access, use this
  [link](https://join.slack.com/t/oakgarden/shared_invite/zt-204sf8w5c-Vxl71cI~WAYhsMLbHGxeMw)
  to join).  We can discuss details or align expectations if you are not familiar
  with the `{oak}` philosophy and programming strategy. The team will try to
  review the issues within the next backlog meeting and give some initial
  feedback. Since we are not a 100% fully resourced software development team it
  might be that some issues will take longer to respond to depending on the amount
  of overall issues.
* Familiarize yourself with our [programming strategy](https://pharmaverse.github.io/admiraldev/main/articles/programming_strategy.html), guidance for [GitHub usage](https://pharmaverse.github.io/admiraldev/main/articles/git_usage.html) and [unit testing](https://pharmaverse.github.io/admiraldev/main/articles/unit_test_guidance.html).
* All newly [created issues](https://github.com/pharmaverse/oak/issues) will be
  reviewed within the next backlog meeting and the creator will receive an
  initial feedback via a comment. Someone from the core development team will
  then triage new issues by assigning the appropriate labels (such as “user
  request” so we can easily identify new requests).
* Issues are meant to be taken on by users from the Pharma programming
  community.

# Contribution Model

## Type 1 Contribution with Code:

* First, the user creates an issue or comments on an existing issue to notify
  that they’d like to contribute code.
* Follow our development process step-by-step guide.
* We advise to contact an `{oak}` core development team directly via [Slack](https://app.slack.com/client/T028PB489D3/C02M8KN8269) before submitting code for complex functionality.

## Type 2 Contribution without Code:

* User creates an issue and ideally contacts an `{oak}` team member via [Slack](https://oakgarden.slack.com).
* The `{oak}` core development team will contact the issue creator as soon
  as possible to discuss further details.

See [Contribution to {admiral}](https://pharmaverse.github.io/admiral/cran-release/articles/contribution_model.html) for additional details.

# Contener guidline

This guideline will walk you through the process of setting up and using the Oak image in both GitHub Codespaces and Visual Studio Code's devcontainer functionality.

## Use GitHub Codespaces


1. **Prerequisites** :

* Have a GitHub account.
* The repository you want to open in Codespaces must be under your account or you must have appropriate permissions.

2. **Open the Repository** :

* Navigate to the desired GitHub repository on GitHub's website.

3. **Start Codespaces** :

* Click on the "Code" button (it has a green color most of the times).
* In the dropdown, you'll find an option named "Open with Codespaces". Click on it.

4. **Choose or Create a Codespace** :

* If you've already created a Codespace for this repository, you'll see it listed. You can click on it to open.
* If not, click on the "+ New codespace" button.

5. **Configure the Environment (If needed)** :

* Depending on the repository, you might have a `.devcontainer` folder which will define the environment. GitHub Codespaces will use the settings defined here to set up the environment.
* If you need to customize or install additional tools, you can do so once the Codespace is launched.

6. **Use Codespaces** :

* Once your environment is set up, you'll have an instance of Visual Studio Code running in your browser, connected to the repository.
* You can now edit, run, and debug your code just like you would in a local environment.

7. **Close the Codespace** :

* When you're done, you can close the browser tab/window.
* Remember, you'll be billed (if you're on a paid plan) for the time your Codespace is running, so it's a good idea to stop or delete it if you're not using it. You can do this from the Codespaces tab in your GitHub repository.

## Use Visual Code devcontainer

1. **Prerequisites** :

* Have a Docker and VS Code installed
* linux/arm64 is supported only for R 4.3

2. **Install devcontainer extension** :

* In the left sidebar, click on the extensions icon (it looks like square blocks or a Lego piece).
* In the search bar, type "Dev Containers" to search for the extension.
* From the search results, locate the "Dev Containers" extension provided by Microsoft and click the 'Install' button.

3. **Open in Container** :

* In VS Code, use the command palette (`Ctrl+Shift+P` or `Cmd+Shift+P` on macOS) and run the "Dev Containers: Rebuild and Reopen in Container" command. VS Code will then set up the devcontainer using the Oak image.

4. **Using the Devcontainer** :

* Once the environment is set up, you can start coding in VS Code as if you were in a local environment, but with the capabilities and tools provided by the Oak image.
