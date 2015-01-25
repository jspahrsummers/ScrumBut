# ScrumBut

ScrumBut is a web app, built with [Yesod](http://www.yesodweb.com), that adds [Scrum](https://www.scrum.org/resources/what-is-scrum/) features to GitHub Issues.

Despite Scrum’s focus on synchronous, co-located sprint meetings, ScrumBut will try to support asynchronous, distributed workflows as much as possible, to make it easy for remote workers to collaborate as well. This is why the project’s name comes from a [derogatory term](https://www.scrum.org/ScrumBut) referring to teams that don’t use “true Scrum.”

## Estimating stories

A particular team could start using one of their repositories by following these steps:

1. Everyone on the team signs in using their GitHub account.
1. One of the collaborators sets up that repository in ScrumBut, enabling Scrum features for it.

Once set up, any issue can be assigned story points as follows:

1. Collaborators submit _story point_ estimates for the issue, along with an optional rationale for their estimate.
1. Once all team members have estimated the issue, a middle ground is automatically chosen if the estimates are close enough to each other.
1. If there are any big outliers in estimation, those users will get a notification and an opportunity to adjust their estimate, or else talk with the rest of the team about adjusting theirs.

## Viewing progress

ScrumBut will treat GitHub milestones as sprints if they contain [estimated](#estimating-stories) issues.

After at least one issue in a milestone has been estimated, anyone can see the total number of story points in that milestone, as well as how many have been completed so far.

After a milestone with estimates has been completed, the total number of points will automatically be incorporated into a _velocity_ that can be used to predict how many story points can be accomplished going forward.

## Deployment

Although there are many possible ways to deploy ScrumBut, the only _supported_ method is the [Haskell on Heroku](https://haskellonheroku.com) buildpack. Please see the [Haskell on Heroku tutorial](https://haskellonheroku.com/tutorial/) to learn how you can deploy your own copy.

## License

ScrumBut is released under the [MIT License](LICENSE.md).
