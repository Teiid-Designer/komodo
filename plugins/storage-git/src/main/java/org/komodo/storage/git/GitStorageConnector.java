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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.RebaseResult;
import org.eclipse.jgit.api.RebaseResult.Status;
import org.eclipse.jgit.api.TransportConfigCallback;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.transport.HttpTransport;
import org.eclipse.jgit.transport.JschConfigSessionFactory;
import org.eclipse.jgit.transport.OpenSshConfig;
import org.eclipse.jgit.transport.OpenSshConfig.Host;
import org.eclipse.jgit.transport.SshSessionFactory;
import org.eclipse.jgit.transport.SshTransport;
import org.eclipse.jgit.transport.Transport;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.util.FS;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnectorId;
import org.komodo.spi.storage.StorageNode;
import org.komodo.spi.storage.StorageParent;
import org.komodo.spi.storage.StorageTree;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

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

    /**
     * The ssh private key
     */
    public static final String REPO_PRIVATE_KEY = "repo-private-key-property";

    /**
     * The ssh passphrase
     */
    public static final String REPO_PASSPHRASE = "repo-passphrase-property";

    /**
     * The known hosts key for this repository
     */
    public static final String REPO_KNOWN_HOSTS_ID = "repo-known-hosts-property";

    /**
     * The password property (used by http)
     */
    public static final String REPO_USERNAME = "repo-username-property";

    /**
     * The password property (used by both ssh and http)
     */
    public static final String REPO_PASSWORD = "repo-password-property";

    /**
     * The name of the author to be applied when writing a commit
     */
    public static final String AUTHOR_NAME_PROPERTY = "author-name-property";

    /**
     * The email of the author to be applied when writing a commit
     */
    public static final String AUTHOR_EMAIL_PROPERTY = "author-email-property";


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
        DESCRIPTORS.add(
                        new Descriptor(
                                      REPO_KNOWN_HOSTS_ID,
                                      false,
                                      true,
                                      "If the repository is secured using ssh then a known hosts id is required to satisfy the first security check." +
                                      "This can be usually be found in the file ~/.ssh/known_hosts after manually connecting to the host using ssh"));
        DESCRIPTORS.add(
                        new Descriptor(
                                      REPO_PRIVATE_KEY,
                                      false,
                                      true,
                                      "If the repository is secured using ssh then a private key is required for authenticating " +
                                      "access to it."));
        DESCRIPTORS.add(
                        new Descriptor(
                                      REPO_PASSPHRASE,
                                      false,
                                      true,
                                      "If the repository is secured using ssh then a private key is required for authenticating. If in" + 
                                      "turn the private key is encrypted with a passphrase then this is also required"));
        DESCRIPTORS.add(
                        new Descriptor(
                                      REPO_USERNAME,
                                      false,
                                      "If the repository is secured using http then a username property will be required for authentication"));
        DESCRIPTORS.add(
                        new Descriptor(
                                      REPO_PASSWORD,
                                      false,
                                      true,
                                      "If the repository is secured using http (or password ssh rather than key-based ssh) then a password property"
                                      + "will be required for authentication"));
        DESCRIPTORS.add(
                        new Descriptor(
                                      AUTHOR_NAME_PROPERTY,
                                      false,
                                      "Specifies the name of the author for commits made during the write operation"));
        DESCRIPTORS.add(
                        new Descriptor(
                                      AUTHOR_EMAIL_PROPERTY,
                                      false,
                                      "Specifies the email address of the author for commits made during the write operation"));
    }

    /**
     * {@link JschConfigSessionFactory} that makes no use of local ssh credential files
     * such as ~/.ssh/id_rsa or ~/.ssh/.known_hosts. All required items are fed in via properties
     */
    private class CustomSshSessionFactory extends JschConfigSessionFactory {

        private JSch customJSch;

        //
        // Overridden to plug in the parameters rather than using credentials from
        // the host filesystem
        //
        @Override
        protected JSch createDefaultJSch(FS fs) throws JSchException {
            JSch jSch = new JSch();

            if (!parameters.containsKey(REPO_PRIVATE_KEY))
                return jSch;

            jSch.removeAllIdentity();

            if (parameters.containsKey(REPO_KNOWN_HOSTS_ID)) {
                String knownHost = parameters.getProperty(REPO_KNOWN_HOSTS_ID);
                ByteArrayInputStream stream = new ByteArrayInputStream(knownHost.getBytes());
                jSch.setKnownHosts(stream);
            }

            String prvKey = parameters.getProperty(REPO_PRIVATE_KEY);
            String passphrase = parameters.getProperty(REPO_PASSPHRASE);

            byte[] prvKeyBytes = prvKey != null ? prvKey.getBytes() : null;
            byte[] pphraseBytes = passphrase != null ? passphrase.getBytes() : null;

            jSch.addIdentity("Identity-" + prvKey.hashCode(), prvKeyBytes, null, pphraseBytes);
            return jSch;
        }

        //
        // Overridden to stop implementation using any credentials stored on the host filesystem
        //
        protected JSch getJSch(final OpenSshConfig.Host hc, FS fs) throws JSchException {
            if (customJSch == null)
                customJSch = createDefaultJSch(fs);

            return customJSch;
        }

        @Override
        protected void configure(Host host, Session session) {
            if (! parameters.containsKey(REPO_PASSWORD))
                return;

            //
            // Should set this if the ssh connection uses passwords rather than public/private key
            //
            session.setPassword(parameters.getProperty(REPO_PASSWORD));
        }
    }

    private class CustomTransportConfigCallback implements TransportConfigCallback {

        @Override
        public void configure(Transport transport) {
            if (transport instanceof SshTransport) {
                //
                // SSH requires an ssh session factory
                //
                SshTransport sshTransport = (SshTransport) transport;
                SshSessionFactory sshSessionFactory = new CustomSshSessionFactory();
                sshTransport.setSshSessionFactory(sshSessionFactory);
            } else if (transport instanceof HttpTransport) {
                //
                // HTTP requires a credentials provider
                //
                HttpTransport httpTransport = (HttpTransport) transport;
                String username = parameters.getProperty(REPO_USERNAME, "no-user-specified");
                String password = parameters.getProperty(REPO_PASSWORD, "no-password-specified");
                UsernamePasswordCredentialsProvider provider =
                        new UsernamePasswordCredentialsProvider(username, password);
                httpTransport.setCredentialsProvider(provider);
            }
        }
    }

    private final Properties parameters;

    private final StorageConnectorId id;

    private Git git;

    private final CustomTransportConfigCallback transportConfigCallback;

    private Set<String> filesForDisposal;

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

        this.transportConfigCallback = new CustomTransportConfigCallback();
    }

    private void addToDisposalCache(File disposalFile) {
        if (filesForDisposal == null)
            filesForDisposal = new HashSet<String>();

        filesForDisposal .add(disposalFile.getAbsolutePath());
    }

    private void cloneRepository() throws Exception {
        File destination = new File(getDestination());
        File destGitDir = new File(destination, ".git");
        if (destGitDir.exists()) {
            git = Git.open(destination);
        } else {
            git = Git.cloneRepository()
                            .setURI(getPath())
                            .setDirectory(destination)
                            .setTransportConfigCallback(transportConfigCallback)
                            .call();
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

        String repoPath = parameters.getProperty(REPO_PATH_PROPERTY);
        String dirName = "cloned-repo" + repoPath.hashCode();
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
        git.fetch()
            .setTransportConfigCallback(transportConfigCallback)
            .call();

        // Ensure the original branch is checked out
        git.checkout()
            .setName(getBranch())
            .setForce(true)
            .call();

        // Rebase the branch against the remote branch
        RebaseResult rebaseResult = git.rebase().setUpstream("origin" + FORWARD_SLASH + getBranch()).call();
        Status status = rebaseResult.getStatus();
        return status.isSuccessful();
    }

    private String directory(String path, DocumentType documentType) {
        if (! path.endsWith(documentType.toString()))
            return path;

        return path.substring(0, path.lastIndexOf(DOT + documentType.toString()));
    }

    @Override
    public void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception {
        ArgCheck.isNotNull(parameters);
        String destination = getFilePath(parameters);
        ArgCheck.isNotEmpty(destination);

        cloneRepository();

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

        File destFile;
        DocumentType documentType = artifact.getDocumentType(transaction);
        if (DocumentType.ZIP.equals(documentType)) {
            //
            // Do not want to add binary zip files to a git repository
            //
            destination = directory(destination, documentType);
            destFile = new File(git.getRepository().getWorkTree(), destination);

            Files.createDirectories(destFile.toPath());
            ByteArrayInputStream byteStream = new ByteArrayInputStream(contents);
            FileUtils.zipExtract(byteStream, destFile);
        }
        else {
            destFile = new File(git.getRepository().getWorkTree(), destination);
            FileUtils.write(contents, destFile);
        }

        // Stage the file(s) for committing
        git.add()
            .addFilepattern(destination)
            .call();

        //
        // Commit the file(s)
        //
        String author = parameters.getProperty(GitStorageConnector.AUTHOR_NAME_PROPERTY, "anonymous");
        String authorEmail = parameters.getProperty(GitStorageConnector.AUTHOR_EMAIL_PROPERTY, "anon@komodo.org");
        RevCommit mergeCommit = git.commit()
                                                                .setAuthor(author, authorEmail)
                                                                .setCommitter(author, authorEmail)
                                                                .setMessage("Change to artifact " + artifactName + " at " + timestamp)
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
            .setTransportConfigCallback(transportConfigCallback)
            .call();
    }

    @Override
    public InputStream read(Properties parameters) throws Exception {
        cloneRepository();

        String fileRef = getFilePath(parameters);
        ArgCheck.isNotNull(fileRef, "RelativeFileRef");

        File gitFile = new File(git.getRepository().getWorkTree(), fileRef);
        if (! gitFile.exists())
            throw new FileNotFoundException();

        FileInputStream fileStream;
        if (gitFile.isDirectory()) {
            File zipFileDest = File.createTempFile(gitFile.getName(), ZIP_SUFFIX);
            File zipFile = FileUtils.zipFromDirectory(gitFile, zipFileDest);
            addToDisposalCache(zipFile);
            fileStream = new FileInputStream(zipFile);
        } else {
            fileStream = new FileInputStream(gitFile);
        }

        return fileStream;
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
        if (git != null)
            git.close();

        String destination = getDestination();
        File destFile = new File(destination);
        if (destFile.exists())
            FileUtils.removeDirectoryAndChildren(destFile);

        if (filesForDisposal != null) {
            for (String filePath : filesForDisposal) {
                File file = new File(filePath);
                if (! file.exists())
                    continue;

                file.delete();
            }

            filesForDisposal = null;
        }
    }
}
