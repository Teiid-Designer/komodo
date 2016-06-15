/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.storage.git;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.RebaseResult;
import org.eclipse.jgit.api.RebaseResult.Status;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnectorId;
import org.komodo.spi.storage.StorageNode;
import org.komodo.spi.storage.StorageParent;
import org.komodo.spi.storage.StorageTree;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;

public class GitStorageConnector implements StorageConnector {

    /**
     * The path to the remote repository
     */
    public static final String REPO_PATH_PROPERTY = "repo-path-property";

    /**
     * The destination into which to clone the repository
     */
    public static final String REPO_DEST_PROPERTY = "repo-dest-property";

    /**
     * The branch to checkout
     */
    public static final String REPO_BRANCH_PROPERTY = "repo-branch-property";

    static final Set<Descriptor> DESCRIPTORS = new HashSet<>();

    static {
        DESCRIPTORS.add(
                       new Descriptor(
                                     REPO_PATH_PROPERTY,
                                     true,
                                     "The URL location of the git repository to use as the destination storage."));
        DESCRIPTORS.add(
                       new Descriptor(
                                     REPO_DEST_PROPERTY,
                                     false,
                                     "The repository will be cloned to the path specified (on the server) by this property. " +
                                     "If not specified then a directory will be created beneath the server's temp directory"));
        DESCRIPTORS.add(
                       new Descriptor(
                                     REPO_BRANCH_PROPERTY,
                                     false,
                                     "The branch that should be checked out of the repository. If not specified then the " +
                                     " master branch is assumed."));
        DESCRIPTORS.add(
                       new Descriptor(
                                     StorageConnector.FILE_PATH_PROPERTY,
                                     true,
                                     "The relative (to the directory specified by \"repo-dest-property\") path of the file. " +
                                     "It is enough to specify only the name of the file."));
    }

    private final Properties parameters;

    private final StorageConnectorId id;

    private Git git;

    public GitStorageConnector(Properties parameters) {
        ArgCheck.isNotNull(parameters);
        ArgCheck.isNotEmpty(parameters.getProperty(REPO_PATH_PROPERTY));

        this.parameters = parameters;

        this.id = new StorageConnectorId() {

            @Override
            public String type() {
                return StorageServiceImpl.STORAGE_ID;
            }

            @Override
            public String location() {
                return getPath();
            }
        };
    }

    private void cloneRepository() throws Exception {
        File destination = new File(getDestination());
        if (destination.exists()) {
            git = Git.open(destination);
        } else {
            git = Git.cloneRepository().setURI(getPath()).setDirectory(destination).call();
        }
    }

    @Override
    public StorageConnectorId getId() {
        return id;
    }

    @Override
    public Set<Descriptor> getDescriptors() {
        return DESCRIPTORS;
    }

    /**
     * @return repository path
     */
    public String getPath() {
        return parameters.getProperty(REPO_PATH_PROPERTY);
    }

    /**
     * @return repository branch
     */
    public String getBranch() {
        String branch = parameters.getProperty(REPO_BRANCH_PROPERTY);
        return branch != null ? branch : "master";
    }

    /**
     * @return the destination of the 'local' clone of the repository
     */
    public String getDestination() {
        String localRepoPath = parameters.getProperty(REPO_DEST_PROPERTY);
        if (localRepoPath != null)
            return localRepoPath;

        String dirName = System.currentTimeMillis() + HYPHEN;
        File repoDest = new File(FileUtils.tempDirectory(), dirName);
        repoDest.mkdir();

        localRepoPath = repoDest.getAbsolutePath();
        parameters.setProperty(REPO_DEST_PROPERTY, localRepoPath);

        return localRepoPath;
    }

    /**
     * @param parameters
     * @return the relative file path from the given parameters
     */
    public String getFilePath(Properties parameters) {
        return parameters.getProperty(FILE_PATH_PROPERTY);
    }

    @Override
    public boolean refresh() throws Exception {
        cloneRepository();
        ArgCheck.isNotNull(git);

        // Fetch latest information from remote
        git.fetch().call();

        // Ensure the original branch is checked out
        git.checkout().setName(getBranch()).setForce(true).call();

        // Rebase the branch against the remote branch
        RebaseResult rebaseResult = git.rebase().setUpstream("origin" + FORWARD_SLASH + getBranch()).call();
        Status status = rebaseResult.getStatus();
        return status.isSuccessful();
    }

    @Override
    public void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception {
        ArgCheck.isNotNull(parameters);
        String destination = getFilePath(parameters);
        ArgCheck.isNotEmpty(destination);

        cloneRepository();

        File destFile = new File(git.getRepository().getWorkTree(), destination);

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss_SSS");
        Date now = new Date();
        String timestamp = sdf.format(now);
        String artifactName = artifact.getName(transaction);

        //
        // Checkout a throw away branch for committing then
        // to be merged back onto main.
        //
        String branchName = artifactName + HYPHEN + timestamp;
        git.checkout()
            .setName(branchName)
            .setCreateBranch(true)
            .setForce(true)
            .setStartPoint(getBranch())
            .call();

        //
        // Write the file contents
        //
        byte[] contents = artifact.export(transaction, parameters);
        FileUtils.write(contents, destFile);

        // Stage the file for committing
        git.add().addFilepattern(destination).call();

        //
        // Commit the file
        //
        RevCommit mergeCommit = git.commit()
                                                                .setMessage("Change to artifact " + artifactName + timestamp)
                                                                .call();

        //
        // Commit was successful
        // so checkout the main branch
        //
        git.checkout()
            .setName(getBranch())
            .setForce(true)
            .call();

        //
        // Ensure the later push would succeed by refreshing now
        //
        refresh();

        //
        // Merge the branch into the main branch
        //
        git.merge()
            .include(mergeCommit)
            .call();

        //
        // Push the change back to the remote
        //
        git.push()
            .call();
    }

    @Override
    public InputStream read(Properties parameters) throws Exception {
        cloneRepository();

        String fileRef = getFilePath(parameters);
        ArgCheck.isNotNull(fileRef, "RelativeFileRef");

        File destFile = new File(git.getRepository().getWorkTree(), fileRef);
        if (destFile.exists())
            return new FileInputStream(destFile);

        throw new FileNotFoundException();
    }

    @Override
    public StorageTree<String> browse() throws Exception {
        cloneRepository();

        StorageTree<String> storageTree = new StorageTree<String>();

        Repository repository = git.getRepository();
        Ref head = repository.findRef(Constants.HEAD);

        try (RevWalk walk = new RevWalk(repository)) {
            RevCommit commit = walk.parseCommit(head.getObjectId());
            RevTree tree = commit.getTree();

            try (TreeWalk treeWalk = new TreeWalk(repository)) {
                treeWalk.addTree(tree);
                treeWalk.setRecursive(false);

                StorageParent<String> parent = storageTree;
                int currentDepth = 0;
                while (treeWalk.next()) {
                    if (treeWalk.getDepth() < currentDepth) {
                        //
                        // Moved back up from a subtree so
                        // decrement currentDepth and
                        // change the parent to its parent
                        //
                        currentDepth = treeWalk.getDepth();
                        parent = parent.getParent();
                    }

                    StorageNode<String> child = parent.addChild(treeWalk.getNameString());

                    if (treeWalk.isSubtree()) {
                        //
                        // Entering a subtree so change
                        // parent to the child and
                        // increment the currentDepth
                        //
                        parent = child;
                        treeWalk.enterSubtree();
                        currentDepth = treeWalk.getDepth();
                    }
                }
            }
        }

        return storageTree;
    }

    @Override
    public void dispose() {
        git.close();
    }

}
