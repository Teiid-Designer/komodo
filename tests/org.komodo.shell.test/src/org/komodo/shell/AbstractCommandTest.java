package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.komodo.core.KEngine;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

/**
 * AbstractCommandTest
 */
@SuppressWarnings({"javadoc", "nls"})
public abstract class AbstractCommandTest extends AbstractLocalRepositoryTest {

    private static KEngine kEngine = KEngine.getInstance();
    
	private ShellCommandFactory factory;
	private ShellCommandReader reader;
	private Writer writer;
	private Writer commandWriter;
	private Class testedCommandClass;
    protected WorkspaceStatusImpl wsStatus;

    /**
     * @param kEngine 
     * @throws Exception 
     */
    @BeforeClass
    public static void startKEngine() throws Exception {
        assertNotNull(_repo);

        kEngine.setDefaultRepository(_repo);
        kEngine.start();

        assertEquals(RepositoryClient.State.STARTED, kEngine.getState());
        assertEquals(Repository.State.REACHABLE, kEngine.getDefaultRepository().getState());
    }

    /**
     * @throws Exception
     */
    @AfterClass
    public static void stopKEngine() throws Exception {
        assertNotNull(kEngine);

        // Reset the latch to signal when repo has been shutdown
        _repoObserver.resetLatch();
        kEngine.shutdown();

        if (! _repoObserver.getLatch().await(1, TimeUnit.MINUTES)) {
            throw new RuntimeException("Local repository was not stopped");
        }

        kEngine.setDefaultRepository(null);
        assertEquals(RepositoryClient.State.SHUTDOWN, kEngine.getState());
        assertEquals(Repository.State.NOT_REACHABLE, _repo.getState());
    }

	/**
	 * Setup the test
	 * @param commandFile the file containing the command
	 * @param commandClass the command being tested
	 * @throws Exception 
	 */
	public void setup(String commandFile, Class commandClass) throws Exception {
	    assertEquals(RepositoryClient.State.STARTED, kEngine.getState());
        assertEquals(Repository.State.REACHABLE, kEngine.getDefaultRepository().getState());

        wsStatus = new WorkspaceStatusImpl(kEngine, System.in, System.out);

		this.factory = new ShellCommandFactory(wsStatus);
		this.testedCommandClass = commandClass;
		
    	try {
    		String commandFilePath = "./resources/" + commandFile; //$NON-NLS-1$
    		//String commandFilePath = UnitTestUtil.getTestDataPath() + "../resources" + commandFile;
    		String[] args = new String[]{"-f", commandFilePath}; //$NON-NLS-1$
    		
    		// Create the FileReader
			reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
	        reader.open();
	        
	        // Writer to store the output
	        writer = new StringWriter();
	        commandWriter = new StringWriter();
		} catch (Exception e) {
			Assert.fail("Failed - setup error: "+e.getMessage()); //$NON-NLS-1$
		}
	}

    /**
	 * Teardown the test
     * @throws Exception 
	 */
	@After
	public void teardown( ) throws Exception {
		try {
			this.reader.close();
		} catch (IOException e) {
		    Assert.fail(e.getLocalizedMessage());
		}
		reader = null;
		this.factory = null;
		this.writer = null;
		this.commandWriter = null;
	}
	
	/**
	 * Executes the command file, putting the result output into writer
	 */
	public void execute( ) {
		boolean done = false;
		while (!done) {
			ShellCommand command = null;
			try {
				command = this.reader.read();
				if(command==null || command instanceof ExitCommand) break;
				
				if(command.getClass().getName().equals(testedCommandClass.getName())) {
					command.setOutput(this.commandWriter);
				} else {
					command.setOutput(this.writer);
				}

				boolean success = command.execute();

				if (!success && reader.isBatch()) {
					Assert.fail("Command " + command.getName() + " execution failed"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			} catch (InvalidCommandArgumentException e) {
				Assert.fail("Failed - invalid command: "+e.getMessage()); //$NON-NLS-1$
			} catch (Exception e) {
				Assert.fail("Failed : "+e.getMessage()); //$NON-NLS-1$
			}
		}
	}
	
	/**
	 * Get command output.  Contains only the output of the command being tested.
	 * @return the output
	 */
	public String getCommandOutput() {
		return this.commandWriter.toString();
	}
	
	/**
	 * Get the message indent string
	 * @return the indent string
	 */
	public static String getIndentStr() {
		int indent = CompletionConstants.MESSAGE_INDENT;
		StringBuffer sb = new StringBuffer();
		for(int i=0; i<indent; i++) {
			sb.append(StringConstants.SPACE);
		}
		return sb.toString();
	}

		 
}
