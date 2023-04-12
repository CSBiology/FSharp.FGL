module ReleaseNotesTasks

open Fake.Extensions.Release
open BlackFox.Fake

/// This might not be necessary, mostly useful for apps which want to display current version as it creates an accessible F# version script from RELEASE_NOTES.md
let createAssemblyVersion = BuildTask.create "createvfs" [] {
    AssemblyVersion.create ProjectInfo.project
}

// https://github.com/Freymaurer/Fake.Extensions.Release#releaseupdate
let updateReleaseNotes = BuildTask.createFn "ReleaseNotes" [] (fun config ->
    ReleaseNotes.update(ProjectInfo.gitOwner, ProjectInfo.project, config)
)


// https://github.com/Freymaurer/Fake.Extensions.Release#githubdraft
let githubDraft = BuildTask.createFn "GithubDraft" [] (fun config ->

    let body = "We are ready to go for the first release!"

    Github.draft(
        ProjectInfo.gitOwner,
        ProjectInfo.project,
        (Some body),
        None,
        config
    )
)