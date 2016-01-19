/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.utils;

/**
 * Represents a translated message.
 */
public class LocalizedMessage {

    private final String id;
    private final String localeCode;
    private final String message;

    /**
     * @param msgId
     *        the message identifier (cannot be empty)
     * @param msgLocale
     *        the message locale code (cannot be empty)
     * @param msgText
     *        the translated message (cannot be empty)
     */
    public LocalizedMessage( final String msgId,
                             final String msgLocale,
                             final String msgText ) {
        this.id = msgId;
        this.localeCode = msgLocale;
        this.message = msgText;
    }

    /**
     * @return the identifier (never empty)
     */
    public String getId() {
        return this.id;
    }

    /**
     * @return the locale code (never empty)
     */
    public String getLocaleCode() {
        return this.localeCode;
    }

    /**
     * @return the translated message (never empty)
     */
    public String getMessage() {
        return this.message;
    }

}
