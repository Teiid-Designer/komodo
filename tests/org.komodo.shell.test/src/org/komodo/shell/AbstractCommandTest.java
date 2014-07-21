package org.komodo.shell;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import org.junit.Assert;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.spi.constants.StringConstants;

/**
 * AbstractCommandTest
 */
public abstract class AbstractCommandTest {

	private ShellCommandFactory factory;
	private ShellCommandReader reader;
	private Writer writer;
	private Writer commandWriter;
	private Class testedCommandClass;

	/**
	 * Setup the test
	 * @param commandFile the file containing the command
	 * @param commandClass the command being tested
	 * @param wsStatus workspace status object
	 */
	public void setup(String commandFile, Class commandClass,  WorkspaceStatus wsStatus) {
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
	 */
	public void teardown( ) {
		try {
			this.reader.close();
		} catch (IOException e) {
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
