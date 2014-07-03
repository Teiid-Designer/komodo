package org.komodo.shell;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import org.junit.Assert;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ExitCommand;

/**
 * AbstractCommandTest
 */
public abstract class AbstractCommandTest {

	private ShellCommandFactory factory;
	private ShellCommandReader reader;
	private Writer writer;

	/**
	 * Setup the test
	 * @param commandFile the file containing the command
	 * @param wsStatus workspace status object
	 */
	public void setup(String commandFile, WorkspaceStatus wsStatus) {
		this.factory = new ShellCommandFactory(wsStatus);
		
    	try {
    		String commandFilePath = "./resources/" + commandFile; //$NON-NLS-1$
    		//String commandFilePath = UnitTestUtil.getTestDataPath() + "../resources" + commandFile;
    		String[] args = new String[]{"-f", commandFilePath}; //$NON-NLS-1$
    		
    		// Create the FileReader
			reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
	        reader.open();
	        
	        // Writer to store the output
	        writer = new StringWriter();
		} catch (IOException e) {
			Assert.fail("Failed - setup error: "+e.getMessage()); //$NON-NLS-1$
		}
	}
	
	/**
	 * Teardown the test
	 */
	public void teardown( ) {
		try {
			this.reader.close();
		} catch (IOException e) {
		}
		reader = null;
		this.factory = null;
		this.writer = null;
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
				
				command.setOutput(this.writer);

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
	 * Get writer output string
	 * @return the output
	 */
	public String getWriterOutput() {
		return this.writer.toString();
	}
		 
}
