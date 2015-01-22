package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.CountDownLatch;
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
import org.komodo.spi.repository.RepositoryObserver;

/**
 * AbstractCommandTest
 */
public abstract class AbstractCommandTest {

    private static KEngine kEngine = KEngine.getInstance();

    private static RepositoryObserver stateObserver;
    
	private ShellCommandFactory factory;
	private ShellCommandReader reader;
	private Writer writer;
	private Writer commandWriter;
	private Class testedCommandClass;
    protected WorkspaceStatusImpl wsStatus;
    

    private static CountDownLatch createWaitingLatch() {
        final Repository defaultRepo = kEngine.getDefaultRepository();

        // Latch for awaiting the change of state of the default repository
        final CountDownLatch stateLatch = new CountDownLatch(1);

        if (stateObserver != null)
            defaultRepo.removeObserver(stateObserver);

        // Observer attached to the default repository for listening for the change of state
        stateObserver = new RepositoryObserver() {

            @Override
            public void stateChanged() {
                stateLatch.countDown();
            }
        };
        defaultRepo.addObserver(stateObserver);
        return stateLatch;
    }

    /**
     * @param kEngine 
     * @throws Exception 
     */
    @BeforeClass
    public static void startKEngine() throws Exception {
        CountDownLatch waitingLatch = createWaitingLatch();

        kEngine.start();

        // Block the thread until the latch has counted down or timeout has been reached
        assertTrue(waitingLatch.await(3, TimeUnit.MINUTES));
        kEngine.getDefaultRepository().removeObserver(stateObserver);

        assertEquals(RepositoryClient.State.STARTED, kEngine.getState());
        assertEquals(Repository.State.REACHABLE, kEngine.getDefaultRepository().getState());
    }

    /**
     * @throws Exception
     */
    @AfterClass
    public static void stopKEngine() throws Exception {
        assertNotNull(kEngine);
        CountDownLatch waitingLatch = createWaitingLatch();

        kEngine.shutdown();

        // Block the thread until the latch has counted down or timeout has been reached
        assertTrue(waitingLatch.await(3, TimeUnit.MINUTES));
        kEngine.getDefaultRepository().removeObserver(stateObserver);

        assertEquals(RepositoryClient.State.SHUTDOWN, kEngine.getState());
        assertEquals(Repository.State.NOT_REACHABLE, kEngine.getDefaultRepository().getState());
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
					Assert.fail("Command Execution Failed"); //$NON-NLS-1$
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
